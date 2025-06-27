package llmaam.frontend.scm

// Adapted from code by Yuxuan Chen

import scala.util.parsing.combinator._
import scala.io.Source

object SchemeParser extends SchemeTokenParser {
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

  def apply(input: String): Option[Expr] = apply(program, input)

  def apply[T](pattern: Parser[T], input: String): Option[T] = parse(pattern, input) match {
    case Success(matched, _) => Some(matched)
    case e => println(e); None
  }
}
