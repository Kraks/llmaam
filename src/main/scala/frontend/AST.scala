package llmaam.frontend.scm

// TODO: refactor/unify these Scheme AST to Syntax.scala in the parent folder

trait Expr
trait AtomExpr

case class SSym(x: String) extends Expr with AtomExpr {
  override def toString: String = "SSym(\"" + x + "\")"
}

case class Var(x: String) extends Expr with AtomExpr {
  override def toString: String = "Var(\"" + x + "\")"
}
case class App(e1: Expr, param: List[Expr]) extends Expr with AtomExpr
case class Lam(param: List[String], body: Expr) extends Expr with AtomExpr {
  override def toString: String = {
    val list = param map { s => "\"" + s + "\"" } mkString (", ")
    "Lam(List(" + list + "), " + body + ")"
  }
}

case class Bind(x: String, e: Expr) {
  override def toString: String = "Bind(\"" + x + "\" " + e + ")"
  def toSet: Set_! = Set_!(x, e)
}

case class Let(bds: List[Bind], body: Expr) extends Expr {
  def toApp: App = {
    val args = bds map { case Bind(name, _) => name }
    val vals = bds map { case Bind(_, value) => value }
    App(Lam(args, body), vals)
  }
}

case class Lrc(bds: List[Bind], body: Expr) extends Expr {
  def toApp: App =
    Let(bds.map { case Bind(x, _) => Bind(x, Void()) },
      Begin(bds.map(_.toSet) ++ List(body))).toApp
}

case class IntLit(x: Int) extends Expr with AtomExpr
case class FloatLit(x: Double) extends Expr with AtomExpr
case class BoolLit(x: Boolean) extends Expr with AtomExpr
case class CharLit(x: Char) extends Expr with AtomExpr {
  override def toString: String = "CharLit('" + x + "')"
}
case class If(cnd: Expr, thn: Expr, els: Expr) extends Expr with AtomExpr

trait CondBrTrait { val cnd: Expr; val thn: Expr }
case class CondBr(cnd: Expr, thn: Expr) extends CondBrTrait
case class CondProcBr(cnd: Expr, thn: Expr) extends CondBrTrait
case class Cond(branches: List[CondBrTrait]) extends Expr

case class CaseBranch(cases: List[Expr], thn: Expr)
case class Case(e: Expr, branches: List[CaseBranch]) extends Expr

case class Void() extends Expr with AtomExpr
case class Set_!(x: String, e: Expr) extends Expr with AtomExpr {
  override def toString: String = "Set_!(\"" + x + "\", " + e + ")"
}
case class Begin(es: List[Expr]) extends Expr
case class Define(x: String, e: Expr) extends Expr {
  override def toString: String = "Define(\"" + x + "\"," + e + ")"
}

extension (e: Expr)
  def pretty: String = e match
    case Var(x) => x
    case Void() => "(void)"
    case SSym(x) => "'" + x
    case CharLit(x)   => "#\\" + x
    case IntLit(x)    => x.toString
    case FloatLit(x)  => x.toString
    case BoolLit(x)   => if (x) "#t" else "#f"
    case Set_!(x, e)  => s"(set! $x ${e.pretty})"
    case Define(x, e) => s"(define $x ${e.pretty})"
    case App(x, l)    => s"(${(x::l).map(_.pretty).mkString(" ")})"
    case Begin(es)    => s"(begin ${es.map(_.pretty).mkString(" ")})"
    case If(c, t, e)  => s"(if ${c.pretty} ${t.pretty} ${e.pretty})"
    case Lam(params, body) => s"(lambda (${params.mkString(" ")}) ${body.pretty})"

  def prettyPrint: Unit = println(e.pretty)

  def size: Int = e match {
    case Var(x) => 1
    case Void() => 1
    case SSym(x) => 1
    case CharLit(x)   => 1
    case IntLit(x)    => 1
    case FloatLit(x)  => 1
    case BoolLit(x)   => 1
    case Set_!(x, e)  => 1 + e.size
    case Define(x, e) => 1 + e.size
    case App(x, l)    => 1 + x.size + l.foldLeft(0)(_ + _.size)
    case Begin(es)    => 1 + es.foldLeft(0)(_ + _.size)
    case If(c, t, e)  => 1 + c.size + t.size + e.size
    case Lam(params, body) => 1 + body.size
  }

  def defVars: Set[String] = e match {
    case Define(x, e) => Set(x) ++ e.defVars
    case Set_!(x, e)  => e.defVars
    case App(x, l)    => x.defVars ++ l.map(_.defVars).foldLeft(Set[String]())(_ ++ _)
    case Begin(es)    => es.map(_.defVars).foldLeft(Set[String]())(_ ++ _)
    case If(c, t, e)  => c.defVars ++ t.defVars ++ e.defVars
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
      case Set_!(x, e)  => e.free
      case Define(x, e) => e.free - x
      case App(x, l)    => x.free ++ l.map(_.free).foldLeft(Set[String]())(_ ++ _)
      case Begin(es)    => es.map(_.free).foldLeft(Set[String]())(_ ++ _)
      case If(c, t, e)  => c.free ++ t.free ++ e.free
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
      case Set_!(x, e)  =>
        val (ne, nm) = alpha(e, map)
        (Set_!(map(x), ne), map)
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
      case Lam(params, body) =>
        val nparams = params.map(x => (x, fresh("var")))
        val nmap = map ++ nparams.toMap
        val (nbody, _) = alpha(body, nmap)
        (Lam(nparams.map(_._2), nbody), map)
    }
  }
}
