package llmaam.frontend.scm

// Scheme reader AST, normalized into ANF before analysis.

trait Expr
trait AtomExpr

case class SSym(x: String) extends Expr with AtomExpr
case class Var(x: String) extends Expr with AtomExpr
case class App(e1: Expr, param: List[Expr]) extends Expr with AtomExpr
case class Lam(param: List[String], body: Expr) extends Expr with AtomExpr
case class RestLam(params: List[String], rest: String, body: Expr) extends Expr
case class SyntaxForm(datum: Datum) extends Expr
case class Bind(x: String, e: Expr) {
  def toSet = SetVar(x, e)
}
case class Let(bds: List[Bind], body: Expr) extends Expr
case class Letrec(bds: List[Bind], body: Expr) extends Expr

case class IntLit(x: Int) extends Expr with AtomExpr
case class FloatLit(x: Double) extends Expr with AtomExpr
case class BoolLit(x: Boolean) extends Expr with AtomExpr
case class CharLit(x: Char) extends Expr with AtomExpr
case class If(cnd: Expr, thn: Expr, els: Expr) extends Expr with AtomExpr

sealed trait CondBrTrait
case class CondTest(cnd: Expr) extends CondBrTrait
case class CondBr(cnd: Expr, thn: Expr) extends CondBrTrait
case class CondProcBr(cnd: Expr, thn: Expr) extends CondBrTrait
case class Cond(branches: List[CondBrTrait]) extends Expr

case class CaseBranch(cases: List[Expr], thn: Expr)
case class Case(e: Expr, branches: List[CaseBranch], otherwise: Expr = Void()) extends Expr

case class Void() extends Expr with AtomExpr
case class SetVar(x: String, e: Expr) extends Expr with AtomExpr
case class Begin(es: List[Expr]) extends Expr
case class Define(x: String, e: Expr) extends Expr

private def renderScheme(e: Expr, generatedPrefix: String): String =
  def name(x: String): String =
    if x.startsWith("#:") then generatedPrefix + x.drop(2) else x

  def render(x: Expr): String = renderScheme(x, generatedPrefix)
  def parens(xs: List[String]): String = xs.mkString("(", " ", ")")
  def bindings(bs: List[Bind]): String = parens(bs.map(b => parens(List(name(b.x), render(b.e)))))

  // A case key is already in datum context. Undo the reader's list, pair and vector encoding.
  def datum(x: Expr): String = x match
    case SSym(s) => s
    case App(Var("list"), xs) => parens(xs.map(datum))
    case App(Var("cons"), List(a, d)) => s"(${datum(a)} . ${datum(d)})"
    case App(Var("vector"), xs) => "#" + parens(xs.map(datum))
    case other => render(other)

  e match
    case SyntaxForm(d) => d.pretty
    case Var(x) => name(x)
    case Void() => "(void)"
    case SSym(x) => "'" + x
    case CharLit(' ') => "#\\space"
    case CharLit('\n') => "#\\newline"
    case CharLit(c) => "#\\" + c
    case IntLit(n) => n.toString
    case FloatLit(n) => n.toString
    case BoolLit(b) => if b then "#t" else "#f"
    case SetVar(x, rhs) => s"(set! ${name(x)} ${render(rhs)})"
    case Define(x, rhs) => s"(define ${name(x)} ${render(rhs)})"
    case App(f, args) => parens(render(f) :: args.map(render))
    case Begin(es) => parens("begin" :: es.map(render))
    case If(c, t, f) => s"(if ${render(c)} ${render(t)} ${render(f)})"
    case Lam(params, body) => s"(lambda ${parens(params.map(name))} ${render(body)})"
    case RestLam(params, rest, body) =>
      val formals = if params.isEmpty then name(rest) else parens(params.map(name) ++ List(".", name(rest)))
      s"(lambda $formals ${render(body)})"
    case Let(bs, body) => s"(let ${bindings(bs)} ${render(body)})"
    case Letrec(bs, body) => s"(letrec ${bindings(bs)} ${render(body)})"
    case Cond(bs) =>
      val last = bs.length - 1
      val clauses = bs.zipWithIndex.map {
        case (CondBr(BoolLit(true), rhs), `last`) => s"(else ${render(rhs)})"
        case (CondBr(c, rhs), _) => s"(${render(c)} ${render(rhs)})"
        case (CondProcBr(c, f), _) => s"(${render(c)} => ${render(f)})"
        case (CondTest(c), _) => s"(${render(c)})"
      }
      parens("cond" :: clauses)
    case Case(key, bs, otherwise) =>
      val clauses = bs.map(b => s"(${parens(b.cases.map(datum))} ${render(b.thn)})")
      val fallback = Option.when(otherwise != Void())(s"(else ${render(otherwise)})")
      parens("case" :: render(key) :: (clauses ++ fallback))

extension (e: Expr)
  def pretty: String =
    val text = renderScheme(e, "#:")
    if !text.contains("#:") then text
    else
      // Choose one legal prefix absent from the whole tree, including macro templates.
      val prefix = Iterator.iterate("$tmp-")("$" + _).find(p => !text.contains(p)).get
      renderScheme(e, prefix)

  def prettyPrint: Unit = println(e.pretty)

  def size: Int = e match {
    case Var(x) => 1
    case Void() => 1
    case SSym(x) => 1
    case CharLit(x)   => 1
    case IntLit(x)    => 1
    case FloatLit(x)  => 1
    case BoolLit(x)   => 1
    case SetVar(x, e)  => 1 + e.size
    case Define(x, e) => 1 + e.size
    case App(x, l)    => 1 + x.size + l.foldLeft(0)(_ + _.size)
    case Begin(es)    => 1 + es.foldLeft(0)(_ + _.size)
    case If(c, t, e)  => 1 + c.size + t.size + e.size
    case RestLam(_, _, body) => 1 + body.size
    case SyntaxForm(_) => 1
    case Lam(params, body) => 1 + body.size
  }

  def defVars: Set[String] = e match {
    case Define(x, e) => Set(x) ++ e.defVars
    case SetVar(x, e)  => e.defVars
    case App(x, l)    => x.defVars ++ l.map(_.defVars).foldLeft(Set[String]())(_ ++ _)
    case Begin(es)    => es.map(_.defVars).foldLeft(Set[String]())(_ ++ _)
    case If(c, t, e)  => c.defVars ++ t.defVars ++ e.defVars
    case RestLam(_, _, body) => body.defVars
    case Lam(params, body) => body.defVars
    case _ => Set()
  }

  def free: Set[String] = {
    val fv: Set[String] = e match {
      case Var(x) => Set(x)
      case Void() => Set()
      case SSym(x) => Set()
      case CharLit(x)   => Set()
      case IntLit(x)    => Set()
      case FloatLit(x)  => Set()
      case BoolLit(x)   => Set()
      case SetVar(x, e)  => e.free
      case Define(x, e) => e.free - x
      case App(x, l)    => x.free ++ l.map(_.free).foldLeft(Set[String]())(_ ++ _)
      case Begin(es)    => es.map(_.free).foldLeft(Set[String]())(_ ++ _)
      case If(c, t, e)  => c.free ++ t.free ++ e.free
      case RestLam(params, rest, body) => body.free -- params - rest
      case SyntaxForm(_) => throw new UnsupportedOperationException("free variables require macro expansion")
      case Lam(params, body) => body.free -- params.toSet
    }
    fv -- e.defVars
  }

object AlphaRenamer {
  var count = 1
  def fresh(pre: String = "var"): String = {
    val name = pre + count.toString
    count = count + 1
    name
  }
  def alpha(e: Expr, map: Map[String, String]): (Expr, Map[String, String]) = {
    e match {
      case Var(x) =>
        val newExpr = if (map.contains(x)) Var(map(x)) else e
        (newExpr, map)
      case Void() => (e, map)
      case SSym(x) => (e, map)
      case CharLit(x)   => (e, map)
      case IntLit(x)    => (e, map)
      case FloatLit(x)  => (e, map)
      case BoolLit(x)   => (e, map)
      case SetVar(x, e)  =>
        val (ne, nm) = alpha(e, map)
        (SetVar(map(x), ne), map)
      case Define(x, e) =>
        val name = if (map.contains(x)) map(x) else fresh()
        val nm = map + (x -> name)
        val (ne, nnm) = alpha(e, nm)
        (Define(name, ne), nm)
      case App(x, l)    => (App(alpha(x, map)._1, l.map(alpha(_, map)._1)), map)
      case Begin(es)    =>
        val defmap = (es.filter(_.isInstanceOf[Define]).map { case Define(x, _) => (x, fresh()) }).toMap
        val (res, mm) = es.foldLeft((List[Expr](), defmap ++ map)) {
          case ((nes, m), e) =>
            val (ne, nm) = alpha(e, m)
            (nes ++ List(ne), nm)
        }
        (Begin(res), map)
      case If(c, t, e)  => (If(alpha(c, map)._1, alpha(t, map)._1, alpha(e, map)._1), map)
      case SyntaxForm(_) => throw new UnsupportedOperationException("alpha renaming requires macro expansion")
      case RestLam(params, rest, body) =>
        val renamed = (params :+ rest).map(x => x -> fresh())
        (RestLam(renamed.init.map(_._2), renamed.last._2, alpha(body, map ++ renamed)._1), map)
      case Lam(params, body) =>
        val nparams = params.map(x => (x, fresh("var")))
        val nmap = map ++ nparams.toMap
        val (nbody, _) = alpha(body, nmap)
        (Lam(nparams.map(_._2), nbody), map)
    }
  }
}
