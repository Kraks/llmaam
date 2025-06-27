package llmaam.frontend.scm

// Adapted from code by Yuxuan Chen

import scala.util.parsing.combinator._
import scala.io.Source

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

trait SchemeParserTrait extends SchemeTokenParser {
  def id[T](x: T) = x

  def variable: Parser[Var] = IDENT ^^ { Var(_) }

  def app: Parser[App] = LPAREN ~> expr ~ expr.* <~ RPAREN ^^ {
    case e ~ param => App(e, param)
  }

  def lam: Parser[Lam] = LPAREN ~> LAMBDA ~> (LPAREN ~> IDENT.* <~ RPAREN) ~ implicit_begin <~ RPAREN ^^ {
    case args ~ body => Lam(args, body)
  }

  def bind: Parser[Bind] = LPAREN ~> IDENT ~ implicit_begin <~ RPAREN ^^ {
    case id ~ e => Bind(id, e)
  }

  def lets: Parser[Expr] = let | letstar | letrec | letproc

  def let: Parser[App] = LPAREN ~> LET ~> (LPAREN ~> bind.+ <~ RPAREN) ~ implicit_begin <~ RPAREN ^^ {
    case binds ~ body => Let(binds, body).toApp
  }

  def letproc: Parser[App] = LPAREN ~> LET ~> IDENT ~ (LPAREN ~> bind.+ <~ RPAREN) ~ implicit_begin <~ RPAREN ^^ {
    case ident ~ binds ~ body =>
      Lrc(List(Bind(ident, Lam(binds.map(_.x), body))), App(Var(ident), binds.map(_.e))).toApp
  }

  def letstar: Parser[App] = LPAREN ~> LETSTAR ~> (LPAREN ~> bind.+ <~ RPAREN) ~ implicit_begin <~ RPAREN ^^ {
    case binds ~ body =>
      binds.dropRight(1).foldRight (Let(List(binds.last), body).toApp) { case (bd, e) => Let(List(bd), e).toApp }
  }

  def letrec: Parser[App] = LPAREN ~> LETREC ~> (LPAREN ~> bind.+ <~ RPAREN) ~ implicit_begin <~ RPAREN ^^ {
    case binds ~ body => Lrc(binds, body).toApp
  }

  def complex: Parser[App] = floatlit ~ floatlit <~ "i" ^^ {
    case r ~ i => App(Var("vector"), List(r, i))
  }

  def intlit: Parser[IntLit] = INT10 ^^ { IntLit(_) }
  def floatlit: Parser[FloatLit] = FLOAT ^^ { FloatLit(_) }
  def boollit: Parser[BoolLit] = (TRUE | FALSE) ^^ { BoolLit(_) }
  def charlit: Parser[CharLit] = CHARLIT ^^ {
    case s => CharLit(s.charAt(2))
  }
  def stringlit: Parser[App] = STRINGLIT ^^ {
    case str =>
      val elements: List[CharLit] = str.toCharArray.map(CharLit(_)).toList
      App(Var("vector"), elements.drop(1).dropRight(1))
  }

  def vecsugar: Parser[App] = VECLPAREN ~> expr.* <~ RPAREN ^^ {
    case elements => App(Var("vector"), elements)
  }

  def literals: Parser[Expr] = complex | floatlit | intlit | charlit | boollit | stringlit | vecsugar

  def ifthel: Parser[If] = LPAREN ~> IF ~> expr ~ expr ~ expr <~ RPAREN ^^ {
    case cond ~ thn ~ els => If(cond, thn, els)
  }

  def condBranch: Parser[CondBr] = LPAREN ~> expr ~ implicit_begin <~ RPAREN ^^ {
    case cond ~ thn => CondBr(cond, thn)
  }
  def condElseBranch: Parser[CondBr] = LPAREN ~> ELSE ~> implicit_begin <~ RPAREN ^^ {
    case thn => CondBr(BoolLit(true), thn)
  }
  def condProcBranch: Parser[CondProcBr] = LPAREN ~> expr ~ (RARROW ~> implicit_begin) <~ RPAREN ^^ {
    case cond ~ proc => CondProcBr(cond, proc)
  }
  def condBranches: Parser[CondBrTrait] = condElseBranch | condBranch | condProcBranch
  def cond: Parser[Cond] = LPAREN ~> COND ~> condBranches.* <~ RPAREN ^^ {
    case branches => Cond(branches)
  }

  def caseBranch: Parser[CaseBranch] = LPAREN ~> (LPAREN ~> expr.* <~ RPAREN) ~ implicit_begin <~ RPAREN ^^ {
    case cases ~ thn => CaseBranch(cases, thn)
  }
  def caseElseBranch: Parser[CaseBranch] = LPAREN ~> ELSE ~> implicit_begin <~ RPAREN ^^ {
    case thn => CaseBranch(List(), thn)
  }
  def cas: Parser[Case] = LPAREN ~> CASE ~> implicit_begin ~ (caseElseBranch | caseBranch).* <~ RPAREN ^^ {
    case ev ~ branches => Case(ev, branches)
  }

  def dispatch: Parser[Expr] = ifthel | cond | cas

  def void: Parser[Void] = LPAREN ~> VOID <~ RPAREN ^^ { _ => Void() }

  def define: Parser[Define] = LPAREN ~> DEF ~> IDENT ~ implicit_begin <~ RPAREN ^^ {
    case id ~ e => Define(id, e)
  }

  def definefunc: Parser[Define] = LPAREN ~> DEF ~> (LPAREN ~> IDENT.+ <~ RPAREN) ~ implicit_begin <~ RPAREN ^^ {
    case idents ~ e => Define(idents.head, Lam(idents.tail, e))
  }

  def set: Parser[Set_!] = LPAREN ~> SET ~> IDENT ~ implicit_begin <~ RPAREN ^^ {
    case id ~ e => Set_!(id, e)
  }

  def begin: Parser[Begin] = LPAREN ~> BEGIN ~> expr.* <~ RPAREN ^^ {
    case exps => Begin(exps)
  }

  def implicit_begin: Parser[Expr] = expr.+ ^^ {
    case e :: Nil => e
    case exps @ (e :: es) => Begin(exps)
  }

  def imp_structure: Parser[Expr] = void | define | definefunc | set | begin

  def quasiquote: Parser[Expr] = QUASIQUOTE ~> quasiterm ^^ id

  def quote: Parser[Expr] = LPAREN ~> QUOTE ~> quasiterm <~ RPAREN ^^ id

  def symbol: Parser[SSym] = SYMBOL ^^ { SSym(_) }

  def list: Parser[App] = LPAREN ~> quasiterm.* <~ RPAREN ^^ {
    case terms => App(Var("list"), terms)
  }
  def unquote: Parser[Expr] = UNQUOTE ~> expr ^^ id

  def quasiterm: Parser[Expr] = literals | unquote | list | symbol

  def expr: Parser[Expr] = literals | quasiquote | quote | variable | lam | lets | dispatch | imp_structure | app

  def program = implicit_begin
}

object SchemeParser extends SchemeParserTrait {
  def apply(input: String): Option[Expr] = apply(program, input)

  def apply[T](pattern: Parser[T], input: String): Option[T] = parse(pattern, input) match {
    case Success(matched, _) => Some(matched)
    case e => println(e); None
  }
}
