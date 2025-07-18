package llmaam.frontend.scm

import llmaam.frontend.scm as S // Scheme AST

import llmaam.syntax.Expr as C // Core Syntax
import llmaam.syntax.{isPrim}

extension (e: S.Expr)
  def toCore: C = AST2Core(e)

object AST2Core:
  // Public entry point.
  def apply(e: S.Expr): C = translate(e.desugar)

  private def unsupported(kind: String, got: Any): Nothing =
    throw new NotImplementedError(s"Translation of $kind is not supported - found: $got")

  // Curry a multi‑argument λ.
  private def curry(params: List[String], body: C): C =
    params.foldRight(body)(C.Lam.apply)

  // Un-curry a function application.
  private def apps(f: C, args: List[C]): C =
    args.foldLeft(f)(C.App.apply)

  // Wrap a list of `S.Bind`s into nested `let`/`letrec` in the target.
  private def foldLets(bds: List[S.Bind], body: S.Expr, rec: Boolean): C =
    val z = translate(body)
    val f = if rec then C.Letrec.apply else C.Let.apply
    bds.foldRight(z){ case (S.Bind(x, rhs), acc) => f(x, translate(rhs), acc) }

  // Translate a sequence that *may* contain `define` statements.
  private def translateSeq(es: List[S.Expr]): C = es match
    // leading define: wrap the *rest* in a Letrec
    case S.Define(x, rhs) :: rest => C.Letrec(x, translate(rhs), translateSeq(rest))
    // A normal expression
    case head :: rest =>
      val headC = translate(head)
      val restC = translateSeq(rest)
      restC match
        case C.Begin(exprs) => C.Begin(headC :: exprs)
        case others => C.Begin(headC :: others :: Nil)
    // empty sequence
    case Nil => C.Begin(Nil)

  def translate(e: S.Expr): C = e match
    case S.IntLit(n)  => C.Lit(n)
    case S.BoolLit(b) => C.Lit(b)
    case S.CharLit(c)  => C.Lit(c)
    case S.FloatLit(d) => C.Lit(d)
    case S.SSym(s) => C.PrimOp("quote", List(C.Lit(s)))
    case S.Void() => C.Void()
    case S.Var(x)  => C.Var(x)
    case S.Lam(params, body) => curry(params, translate(body))
    case S.App(Var(f), args) if isPrim(f) => C.PrimOp(f, args.map(translate))
    case S.App(fun, args) => apps(translate(fun), args.map(translate))
    case S.SetVar(x, rhs) => C.SetVar(x, translate(rhs))
    case S.If(c, t, el) => C.If(translate(c), translate(t), translate(el))
    // Let and Letrec
    // XXX (GW): the parser already desugars `let` and `letrec` into `app`, which is a bit
    // handwavy, should consider refactor there and here.
    case S.Let(bds, body) => foldLets(bds, body, rec = false)
    case S.Lrc(bds, body) => foldLets(bds, body, rec = true)
    // XXX (ZZ): we made a small trick to translate `define`, since we do not have
    // `define` in our core syntax.
    // XXX (GW): I think we might still need `define` in the core syntax.
    // Eg, the following cannot be translated to multiple letrec, but they
    // can be a single letrec with multiple bindings (which we don't have yet either).
    // (begin
    //  (define (f x) (g x))
    //  (define (g x) (+ x 1))
    //  (f 3))
    case S.Define(x, rhs) => C.Letrec(x, translate(rhs), C.Void())
    case S.Begin(es) => translateSeq(es)
