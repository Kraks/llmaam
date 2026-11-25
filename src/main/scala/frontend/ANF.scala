package llmaam.frontend.anf

import llmaam.frontend.scm as S
import llmaam.frontend.counter.fresh

/** ANF expressions are atoms or let bindings whose bodies are expressions.
  * Applications retain Scheme's arity and take only atomic operands.
  * An If computes a value, so its continuation is shared by both branches.
  * Let allocates a binding; SetVar updates an existing binding and returns Void.
  */
sealed trait Expr

enum AExpr extends Expr:
  case Var(x: String)
  case IntLit(x: Int)
  case FloatLit(x: Double)
  case BoolLit(x: Boolean)
  case CharLit(x: Char)
  case Sym(x: String)
  case Void()
  case Lam(params: List[String], body: Expr)
  case RestLam(params: List[String], rest: String, body: Expr)

enum CExpr:
  case Value(value: AExpr)
  case App(fun: AExpr, args: List[AExpr])
  case If(cond: AExpr, thn: Expr, els: Expr)
  case SetVar(x: String, value: AExpr)

case class Let(x: String, rhs: CExpr, body: Expr) extends Expr

/** The shared fresh-name counter is the only mutable state.
  * Evaluation proceeds left to right, including the operator of an application.
  * SyntaxForm must be expanded before normalization.
  */
def normalize(expr: S.Expr): Expr =
  new Normalizer(assignedVariables(expr)).normalize(expr, Map.empty, identity)

// Conservatively track mutable assignment targets so reads survive later operand effects.
private def assignedVariables(expr: S.Expr): Set[String] =
  def combine(es: List[S.Expr]): Set[String] =
    es.foldLeft(Set.empty[String])((assigned, e) => assigned ++ assignedVariables(e))
  expr match
    case S.Lam(_, body) => assignedVariables(body)
    case S.RestLam(_, _, body) => assignedVariables(body)
    case S.App(f, args) => combine(f :: args)
    case S.Let(bs, body) => combine(bs.map(_.e) :+ body)
    case S.Letrec(bs, body) => combine(bs.map(_.e) :+ body) ++ bs.map(_.x)
    case S.If(c, t, f) => combine(List(c, t, f))
    case S.SetVar(x, rhs) => assignedVariables(rhs) + x
    case S.Define(x, rhs) => assignedVariables(rhs) + x
    case S.Begin(es) => combine(es)
    case S.Cond(bs) => combine(bs.flatMap {
      case S.CondTest(c) => List(c)
      case S.CondBr(c, t) => List(c, t)
      case S.CondProcBr(c, f) => List(c, f)
    })
    case S.Case(key, bs, otherwise) =>
      combine(key :: otherwise :: bs.flatMap(b => b.cases :+ b.thn))
    case S.SyntaxForm(d) =>
      throw new IllegalArgumentException(s"ANF requires macro expansion: ${d.pretty}")
    case S.Var(_) | S.IntLit(_) | S.FloatLit(_) | S.BoolLit(_) | S.CharLit(_) | S.SSym(_) | S.Void() =>
      Set.empty
    case other => throw new IllegalArgumentException(s"Unsupported Scheme AST: $other")

private class Normalizer(assigned: Set[String]):
  private type Env = Map[String, String]
  private type Kont = AExpr => Expr

  private def bind(rhs: CExpr, k: Kont): Expr =
    val name = fresh()
    Let(name, rhs, k(AExpr.Var(name)))

  private def renamed(names: List[String]): List[(String, String)] =
    names.map(x => x -> fresh())

  private def reference(x: String, env: Env): String = env.getOrElse(x, x)

  // Each helper threads one continuation. Accumulation uses immutable lists/maps.
  private def operands(es: List[S.Expr], env: Env, k: List[AExpr] => Expr): Expr = es match
    case Nil => k(Nil)
    case e :: rest => normalize(e, env, a => operands(rest, env, as => k(a :: as)))

  private def flatten(es: List[S.Expr]): List[S.Expr] = es.flatMap {
    case S.Begin(inner) => flatten(inner)
    case e => List(e)
  }

  private def sequence(es: List[S.Expr], env: Env, k: Kont): Expr = es match
    case Nil => k(AExpr.Void())
    case S.Define(x, rhs) :: rest =>
      normalize(rhs, env, a => bind(CExpr.SetVar(reference(x, env), a), _ => sequence(rest, env, k)))
    case e :: Nil => normalize(e, env, k)
    case e :: rest => normalize(e, env, _ => sequence(rest, env, k))

  // Allocate the whole recursive group before running any initializer. Definitions
  // in nested begins share their enclosing body, including forward references.
  private def definitions(es: List[S.Expr], env: Env, k: Kont): Expr =
    val flat = flatten(es)
    val bindings = renamed(flat.collect { case S.Define(x, _) => x }.distinct)
    val body = sequence(flat, env ++ bindings, k)
    bindings.foldRight(body) { case ((_, name), rest) =>
      Let(name, CExpr.Value(AExpr.Void()), rest)
    }

  private def cond(bs: List[S.CondBrTrait]): S.Expr = bs match
    case Nil => S.Void()
    case S.CondBr(c, t) :: rest => S.If(c, t, cond(rest))
    case S.CondTest(c) :: rest =>
      val name = fresh()
      S.Let(List(S.Bind(name, c)), S.If(S.Var(name), S.Var(name), cond(rest)))
    case S.CondProcBr(c, f) :: rest =>
      val name = fresh()
      S.Let(List(S.Bind(name, c)), S.If(S.Var(name), S.App(f, List(S.Var(name))), cond(rest)))

  def normalize(e: S.Expr, env: Env, k: Kont): Expr = e match
    case S.IntLit(n) => k(AExpr.IntLit(n))
    case S.FloatLit(n) => k(AExpr.FloatLit(n))
    case S.BoolLit(b) => k(AExpr.BoolLit(b))
    case S.CharLit(c) => k(AExpr.CharLit(c))
    case S.SSym(s) => k(AExpr.Sym(s))
    case S.Void() => k(AExpr.Void())
    case S.Var(x) =>
      val atom = AExpr.Var(reference(x, env))
      // Freeze reads of mutable source variables before later operands can write
      // them. Generated temporaries themselves are never assignment targets.
      if assigned(x) then bind(CExpr.Value(atom), k) else k(atom)
    case S.Lam(params, body) =>
      val bindings = renamed(params)
      k(AExpr.Lam(bindings.map(_._2), normalize(body, env ++ bindings, identity)))
    case S.RestLam(params, rest, body) =>
      val bindings = renamed(params :+ rest)
      k(AExpr.RestLam(bindings.init.map(_._2), bindings.last._2,
        normalize(body, env ++ bindings, identity)))
    case S.App(fun, args) =>
      normalize(fun, env, f => operands(args, env, as => bind(CExpr.App(f, as), k)))
    case S.If(c, t, f) =>
      normalize(c, env, a => bind(CExpr.If(a,
        normalize(t, env, identity), normalize(f, env, identity)), k))
    case S.SetVar(x, rhs) =>
      normalize(rhs, env, a => bind(CExpr.SetVar(reference(x, env), a), k))
    case S.Let(bs, body) =>
      // Parallel let initializers all see the outer environment. Fresh binders
      // prevent a local binding from capturing names in the continuation.
      operands(bs.map(_.e), env, values =>
        val bindings = renamed(bs.map(_.x))
        val result = normalize(body, env ++ bindings, k)
        bindings.zip(values).foldRight(result) { case (((_, name), value), rest) =>
          Let(name, CExpr.Value(value), rest)
        })
    case S.Letrec(bs, body) =>
      val bindings = renamed(bs.map(_.x))
      val inner = env ++ bindings
      val initializers = bs.map(b => S.SetVar(b.x, b.e))
      val result = sequence(initializers :+ body, inner, k)
      bindings.foldRight(result) { case ((_, name), rest) =>
        Let(name, CExpr.Value(AExpr.Void()), rest)
      }
    case S.Begin(es) => definitions(es, env, k)
    case d: S.Define => definitions(List(d), env, k)
    case S.Cond(bs) => normalize(cond(bs), env, k)
    case S.Case(key, bs, otherwise) =>
      val name = fresh()
      val clauses = bs.map { b =>
        val test = b.cases.foldRight[S.Expr](S.BoolLit(false)) { (datum, rest) =>
          S.If(S.App(S.Var("eqv?"), List(S.Var(name), datum)), S.BoolLit(true), rest)
        }
        S.CondBr(test, b.thn)
      }
      normalize(S.Let(List(S.Bind(name, key)),
        S.Cond(clauses :+ S.CondBr(S.BoolLit(true), otherwise))), env, k)
    case S.SyntaxForm(d) =>
      throw new IllegalArgumentException(s"ANF requires macro expansion: ${d.pretty}")
    case other => throw new IllegalArgumentException(s"Unsupported Scheme AST: $other")

extension (e: S.Expr)
  def toANF: Expr = normalize(e)
