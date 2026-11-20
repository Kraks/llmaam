package llmaam.frontend.scm

import scala.io.Source
import scala.util.Using
import scala.util.control.NoStackTrace

object SchemeParser extends SchemeTokenParser:
  import Datum.*

  private case class Invalid(message: String) extends Exception(message) with NoStackTrace
  private def invalid(expected: String, got: Datum): Nothing =
    throw Invalid(s"expected $expected, found ${got.pretty}")

  private val identifier = """[\p{L}\d!$%&*/:<=>?~_^.+@|\-]+""".r
  private val integer = """[+-]?\d+""".r
  private val real = """(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?"""
  private val decimal = ("[+-]?" + real).r
  private val complex = (s"([+-]?$real)([+-]$real)i").r
  private def atom(s: String, quoted: Boolean): Expr = s match
    case "#t" | "#T" => BoolLit(true)
    case "#f" | "#F" => BoolLit(false)
    case "#\\space" => CharLit(' ')
    case "#\\newline" => CharLit('\n')
    case c if c.startsWith("#\\") && c.length == 3 => CharLit(c.last)
    case str if str.startsWith("\"") =>
      val chars = try StringContext.processEscapes(str.tail.init)
                  catch case _: StringContext.InvalidEscapeException => invalid("a string escape", Atom(str))
      App(Var("vector"), chars.toList.map(CharLit.apply))
    case integer() => IntLit(s.toIntOption.getOrElse(invalid("a representable integer", Atom(s))))
    case decimal() => FloatLit(s.toDouble)
    case complex(r, i) => App(Var("vector"), List(FloatLit(r.toDouble), FloatLit(i.toDouble)))
    case identifier() => if quoted then SSym(s) else Var(s)
    case _ => invalid("an identifier or literal", Atom(s))

  private def name(d: Datum): String = d match
    case Atom(s @ identifier()) if !decimal.matches(s) && !complex.matches(s) => s
    case _ => invalid("an identifier", d)

  private def elements(d: Datum): List[Datum] = d match
    case Lst(xs, None) => xs
    case _ => invalid("a proper list", d)

  private def sequence(es: List[Expr]): Expr = es match
    case x :: Nil => x
    case _ => Begin(es)

  private def body(ds: List[Datum]): Expr =
    if ds.isEmpty then throw Invalid("expected a body")
    sequence(ds.map(expression))

  private def lambda(params: Datum, ds: List[Datum]): Expr = params match
    case Lst(xs, None) => Lam(xs.map(name), body(ds))
    case Lst(xs, Some(rest)) => RestLam(xs.map(name), name(rest), body(ds))
    case _ => RestLam(Nil, name(params), body(ds))

  private def bindings(d: Datum): List[Bind] = elements(d).map {
    case Lst(List(x, rhs), None) => Bind(name(x), expression(rhs))
    case other => invalid("a binding (name value)", other)
  }

  // Apply the recursive procedure outside its binding so initializers see the outer scope.
  private def namedLet(loop: String, bs: List[Bind], result: Expr): App =
    val proc = Bind(loop, Lam(bs.map(_.x), result))
    App(Lrc(List(proc), Var(loop)).toApp, bs.map(_.e))

  private def iteration(vars: Datum, exit: Datum, commands: List[Datum]): Expr =
    val steps = elements(vars).map {
      case Lst(x :: init :: step, None) if step.size <= 1 =>
        val id = name(x)
        (Bind(id, expression(init)), step.headOption.fold[Expr](Var(id))(expression))
      case other => invalid("a do binding", other)
    }
    elements(exit) match
      case test :: results =>
        val loop = "#:do"
        val next = App(Var(loop), steps.map(_._2))
        val done = if results.isEmpty then Void() else body(results)
        val repeat = sequence(commands.map(expression) :+ next)
        namedLet(loop, steps.map(_._1), If(expression(test), done, repeat))
      case _ => invalid("a do termination test", exit)

  private def condBranch(d: Datum): CondBrTrait = elements(d) match
    case Atom("else") :: ds => CondBr(BoolLit(true), body(ds))
    case List(test, Atom("=>"), proc) => CondProcBr(expression(test), expression(proc))
    case test :: Atom("=>") :: _ => invalid("a cond recipient", d)
    case List(test) => CondTest(expression(test))
    case test :: ds => CondBr(expression(test), body(ds))
    case _ => invalid("a cond clause", d)

  private def checkElseLast(ds: List[Datum]): Unit =
    if ds.dropRight(1).exists {
      case Lst(Atom("else") :: _, _) => true
      case _ => false
    } then throw Invalid("else must be the last clause")

  private def caseBranch(d: Datum): CaseBranch = elements(d) match
    case keys :: ds => CaseBranch(elements(keys).map(quoted(_, 0)), body(ds))
    case _ => invalid("a case clause", d)

  // Depth zero is ordinary quote. Only escapes at quasiquote depth one are active.
  private def quoted(d: Datum, depth: Int): Expr = d match
    case Atom(s) => atom(s, quoted = true)
    case Lst(List(Atom(op @ ("quasiquote" | "unquote" | "unquote-splicing")), x), None) if depth > 0 =>
      if depth == 1 && op == "unquote" then expression(x)
      else if depth == 1 && op == "unquote-splicing" then invalid("a list or vector element for splicing", d)
      else App(Var("list"), List(SSym(op), quoted(x, if op == "quasiquote" then depth + 1 else depth - 1)))
    case Lst(xs, tail) => quotedList(xs, tail, depth)
    case Vec(xs) => quotedList(xs, None, depth) match
      case App(Var("list"), args) => App(Var("vector"), args)
      case list => App(Var("list->vector"), List(list))

  private def quotedList(xs: List[Datum], tail: Option[Datum], depth: Int): Expr =
    def splice(x: Datum): Option[Datum] = x match
      case Lst(List(Atom("unquote-splicing"), arg), None) if depth == 1 => Some(arg)
      case _ => None

    if tail.isEmpty && !xs.exists(x => splice(x).isDefined) then App(Var("list"), xs.map(quoted(_, depth)))
    else xs.foldRight(tail.fold[Expr](App(Var("list"), Nil))(quoted(_, depth))) { (x, rest) =>
      splice(x) match
        case Some(arg) => App(Var("append"), List(expression(arg), rest))
        case None => App(Var("cons"), List(quoted(x, depth), rest))
    }

  private val special = Set("lambda", "λ", "define", "if", "let", "let*", "letrec", "set!", "do",
    "quote", "quasiquote", "unquote", "unquote-splicing", "case", "when", "unless", "rec", "recur")
  // These dialect-specific forms contain patterns or templates that must survive reading intact.
  private val syntaxForms = Set("define-syntax", "defmacro", "def-macro", "match", "match-let",
    "match-lambda", "match-lambda*", "define-data", "frame")

  private def expression(d: Datum): Expr = d match
    case Atom(s) => atom(s, quoted = false)
    case Vec(_) => quoted(d, 0)
    case Lst(xs, None) => xs match
      case List(Atom("quote"), x) => quoted(x, 0)
      case List(Atom("quasiquote"), x) => quoted(x, 1)
      case Atom("lambda" | "λ") :: params :: ds => lambda(params, ds)
      case Atom("define") :: Lst(f :: xs, rest) :: ds => Define(name(f), lambda(Lst(xs, rest), ds))
      case List(Atom("define"), x, rhs) => Define(name(x), expression(rhs))
      case List(Atom("set!"), x, rhs) => SetVar(name(x), expression(rhs))
      case Atom("begin") :: ds => Begin(ds.map(expression))
      case List(Atom("void")) => Void()
      case Atom("if") :: c :: t :: alt if alt.size <= 1 =>
        If(expression(c), expression(t), alt.headOption.fold[Expr](Void())(expression))
      case Atom("let" | "recur") :: (loop @ Atom(_)) :: bs :: ds => namedLet(name(loop), bindings(bs), body(ds))
      case List(Atom("rec"), loop, rhs) => Lrc(List(Bind(name(loop), expression(rhs))), Var(name(loop))).toApp
      case Atom("let") :: bs :: ds => Let(bindings(bs), body(ds)).toApp
      case Atom("letrec") :: bs :: ds => Lrc(bindings(bs), body(ds)).toApp
      case Atom("let*") :: bs :: ds => bindings(bs).foldRight(body(ds))((b, e) => Let(List(b), e).toApp)
      case Atom("do") :: vars :: exit :: ds => iteration(vars, exit, ds)
      case Atom(op @ ("when" | "unless")) :: c :: ds =>
        if op == "when" then If(expression(c), body(ds), Void()) else If(expression(c), Void(), body(ds))
      case Atom("cond") :: ds =>
        checkElseLast(ds)
        Cond(ds.map(condBranch))
      case Atom("case") :: key :: ds =>
        checkElseLast(ds)
        ds.lastOption match
          case Some(Lst(Atom("else") :: els, None)) => Case(expression(key), ds.init.map(caseBranch), body(els))
          case _ => Case(expression(key), ds.map(caseBranch))
      case Atom("and") :: ds =>
        ds.map(expression).reduceRightOption((x, y) => If(x, y, BoolLit(false))).getOrElse(BoolLit(true))
      case Atom("or") :: ds => ds.map(expression).reduceRightOption { (x, y) =>
        Let(List(Bind("#:or", x)), If(Var("#:or"), Var("#:or"), y)).toApp
      }.getOrElse(BoolLit(false))
      case Atom(op) :: _ if syntaxForms(op) => SyntaxForm(d)
      case Atom(op) :: _ if special(op) => invalid(s"a valid $op form", d)
      case f :: args => App(expression(f), args.map(expression))
      case Nil => quoted(d, 0)
    case _ => invalid("an expression", d)

  private def checked(f: => Expr): Parser[Expr] =
    try success(f) catch case Invalid(message) => err(message)

  def expr: Parser[Expr] = datum >> (d => checked(expression(d)))
  def program: Parser[Expr] = rep(comment) ~> rep(expr <~ rep(comment)) ^^ sequence
  def apply(input: String): Option[Expr] = apply(program, input)
  def apply[T](pattern: Parser[T], input: String): Option[T] = parseAll(pattern, input) match
    case Success(value, _) => Some(value)
    case error: NoSuccess =>
      println(error)
      None

  def parseFile(filename: String): Option[Expr] =
    Using.resource(Source.fromFile(filename))(source => apply(source.mkString))
