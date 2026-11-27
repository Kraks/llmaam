package llmaam.frontend.scm

import munit.FunSuite
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using
import llmaam.frontend.counter.reset

class TestParser extends FunSuite {
  test("pretty-printer"):
    assert(CharLit('a').pretty == "#\\a")

    val e1 = App(Lam(List("x", "y"), App(Var("+"), List(Var("x"), Var("y")))), List(IntLit(1), IntLit(3)))
    assert(e1.pretty == "((lambda (x y) (+ x y)) 1 3)")

    val e2 = If(BoolLit(false), Var("a"), Lam(List("t"), Var("t")))
    assert(e2.pretty == "(if #f a (lambda (t) t))")

  test("pretty-print cond clauses and case datums"):
    val forms = List(
      "(cond)",
      "(cond ((probe)) ((test) => receive) (#t first) (else last))",
      "(case x)",
      "(case x ((less more) (cond (ok))) (((a b) (c . d) #(e f) ()) y) (else z))"
    )
    forms.foreach { source =>
      val ast = SchemeParser(source).get
      assertEquals(ast.pretty, source)
      assertEquals(SchemeParser(ast.pretty), Some(ast))
    }

  test("pretty-print binding forms and named characters"):
    val bs = List(Bind("x", CharLit(' ')), Bind("y", CharLit('\n')))
    val body = App(Var("f"), List(Var("x"), Var("y")))
    assertEquals(Let(bs, body).pretty, "(let ((x #\\space) (y #\\newline)) (f x y))")
    assertEquals(Letrec(bs, body).pretty, "(letrec ((x #\\space) (y #\\newline)) (f x y))")

  test("printing generated identifiers preserves user bindings"):
    reset
    val source = "(lambda ($tmp-or) (or x $tmp-or))"
    val expected = "(lambda ($tmp-or) (let (($$tmp-or0 x)) (if $$tmp-or0 $$tmp-or0 $tmp-or)))"
    assertEquals(SchemeParser(source).get.pretty, expected)
    assertEquals(SchemeParser(expected).get.pretty, expected)

  test("or generates distinct names and consistently references each binding"):
    reset
    val source = "(or (or a b) c d)"
    val inner = Let(List(Bind("#:or0", Var("a"))), If(Var("#:or0"), Var("#:or0"), Var("b")))
    val rest = Let(List(Bind("#:or1", Var("c"))), If(Var("#:or1"), Var("#:or1"), Var("d")))
    val expected = Let(List(Bind("#:or2", inner)), If(Var("#:or2"), Var("#:or2"), rest))
    assertEquals(SchemeParser(source), Some(expected))
    assertNotEquals(SchemeParser(source), Some(expected))

  test("quasiquote"):
    assert(SchemeParser("'xxxx") == Some(SSym("xxxx")))
    assert(SchemeParser("`(a 1 ,(a b))") == Some(App(Var("list"),List(SSym("a"), IntLit(1), App(Var("a"),List(Var("b")))))))

  test("let and letrec preserve binding nodes"):
    val bs = List(Bind("a", IntLit(3)), Bind("b", Var("a")))
    val body = App(Var("add"), List(Var("a"), Var("b")))
    assertEquals(SchemeParser("(let ([a 3] [b a]) (add a b))"), Some(Let(bs, body)))
    assertEquals(SchemeParser("(letrec ([a 3] [b a]) (add a b))"), Some(Letrec(bs, body)))
    assertEquals(SchemeParser("(let* ([a 3] [b a]) (add a b))"),
      Some(Let(List(bs.head), Let(List(bs(1)), body))))
    assertEquals(SchemeParser("(rec f (lambda () f))"),
      Some(Letrec(List(Bind("f", Lam(Nil, Var("f")))), Var("f"))))

  test("implicit"):
    val actual = SchemeParser("(add a b) (add a b)")
    val expected = Some(
      Begin(List(
        App(Var("add"), List(Var("a"), Var("b"))),
        App(Var("add"), List(Var("a"), Var("b")))
      )))
    assert(actual == expected)

  test("bool"):
    val t = "#t"
    val T = "#T"
    val f = "#f"
    val F = "#F"
    assert(SchemeParser(t) == Some(BoolLit(true)))
    assert(SchemeParser(T) == Some(BoolLit(true)))
    assert(SchemeParser(f) == Some(BoolLit(false)))
    assert(SchemeParser(F) == Some(BoolLit(false)))

  test("float"):
    assert(SchemeParser("3.14") == Some(FloatLit(3.14)))
    assert(SchemeParser("-3.14") == Some(FloatLit(-3.14)))
    assert(SchemeParser("0.00000") == Some(FloatLit(0.0)))

  test("complex"):
    assert(SchemeParser("3.14+2.7i") == Some(App(Var("vector"), List(FloatLit(3.14), FloatLit(2.7)))))
    assert(SchemeParser("-3.14-2.7i") == Some(App(Var("vector"), List(FloatLit(-3.14), FloatLit(-2.7)))))

  test("arith"):
    val actual = SchemeParser("(+ (- a b) (* (/ 2 3) 4))")
    val expected = Some(
      App(Var("+"), List(
        App(Var("-"), List(Var("a"), Var("b"))),
        App(Var("*"), List(App(Var("/"), List(IntLit(2), IntLit(3))), IntLit(4))))))
    assert(actual == expected)

  test("define_proc"):
    val actual = SchemeParser("(define (f a b) (+ a b))")
    val expected = Some(Define("f", Lam(List("a", "b"), App(Var("+"), List(Var("a"), Var("b"))))))
    assert(actual == expected)

  test("cond"):
    val actual = SchemeParser(
      """(cond
          [(positive? -5) (error 1)]
          [(zero? -5) (error 2)]
          [(positive? 5) 'here])""")
    val expected = Some(Cond(List(
      CondBr(App(Var("positive?"),List(IntLit(-5))),App(Var("error"),List(IntLit(1)))),
      CondBr(App(Var("zero?"),List(IntLit(-5))),App(Var("error"),List(IntLit(2)))),
      CondBr(App(Var("positive?"),List(IntLit(5))),SSym("here")))))
    assert(actual == expected)

  test("consume whole tokens and whole files"):
    assertEquals(SchemeParser("(if-ready? 1/prod-fac .5)"),
      Some(App(Var("if-ready?"), List(Var("1/prod-fac"), FloatLit(0.5)))))
    List("x (", "x )", "(x]", "(if x)", "(let ((x)) x)", "'(a . b c)", "#true", "x #;").foreach { bad =>
      assert(!SchemeParser.parseAll(SchemeParser.program, bad).successful, bad)
    }

  test("datum comments skip one complete datum, including nested comments"):
    assertEquals(SchemeParser("#;ignored"), Some(Begin(Nil)))
    assertEquals(SchemeParser("#;#;(skip 1) '(skip 2) [keep #;(skip 3) #\\space #\\newline] #;last"),
      Some(App(Var("keep"), List(CharLit(' '), CharLit('\n')))))

  test("quote preserves data and quasiquote activates escapes at the matching depth"):
    val nil = App(Var("list"), Nil)
    def list(xs: Expr*) = App(Var("list"), xs.toList)
    assertEquals(SchemeParser("'(,x . y)"), Some(App(Var("cons"), List(list(SSym("unquote"), SSym("x")), SSym("y")))))
    assertEquals(SchemeParser("`(a ,@xs ,b)"), Some(App(Var("cons"), List(SSym("a"),
      App(Var("append"), List(Var("xs"), App(Var("cons"), List(Var("b"), nil))))))))
    assertEquals(SchemeParser("`(quasiquote (,x))"), Some(list(SSym("quasiquote"), list(list(SSym("unquote"), SSym("x"))))))
    assertEquals(SchemeParser("#(a 1)"), Some(App(Var("vector"), List(SSym("a"), IntLit(1)))))

  test("fixed and rest arguments retain their calling conventions"):
    assertEquals(SchemeParser("(λ args args)"), Some(RestLam(Nil, "args", Var("args"))))
    assertEquals(SchemeParser("(define (f x . xs) xs)"), Some(Define("f", RestLam(List("x"), "xs", Var("xs")))))

  test("macro templates preserve dotted patterns and ellipses"):
    val code = "(define-syntax f (syntax-rules () ((f x ... . tail) (list x ...))))"
    assertEquals(SchemeParser(code).collect { case SyntaxForm(d) => d.pretty }, Some(code))

  test("empty bindings and named let initializers"):
    assertEquals(SchemeParser("(let* () 1 2)"), Some(Begin(List(IntLit(1), IntLit(2)))))
    assertEquals(SchemeParser("(let () 1)"), Some(Let(Nil, IntLit(1))))
    assertEquals(SchemeParser("(letrec () 1)"), Some(Letrec(Nil, IntLit(1))))
    val proc = Letrec(List(Bind("f", Lam(List("x"), Var("x")))), Var("f"))
    assertEquals(SchemeParser("(let f ((x f)) x)"), Some(App(proc, List(Var("f")))))

  test("do uses old bindings in every step and retains an omitted step"):
    reset
    val source = "(do ((x 0 y) (y 1)) (done x) work)"
    val call = App(Var("#:do0"), List(Var("y"), Var("y")))
    val loop = Lam(List("x", "y"), If(Var("done"), Var("x"), Begin(List(Var("work"), call))))
    val proc = Letrec(List(Bind("#:do0", loop)), Var("#:do0"))
    val expected = App(proc, List(IntLit(0), IntLit(1)))
    assertEquals(SchemeParser(source), Some(expected))
    assertNotEquals(SchemeParser(source), Some(expected))

  test("conditional forms preserve values and evaluate tests once"):
    reset
    def lowered(s: String) = SchemeParser(s).map(_.desugar.pretty)
    assertEquals(lowered("(and)"), Some("#t"))
    assertEquals(lowered("(or)"), Some("#f"))
    assertEquals(lowered("(and x y)"), Some("(if x y #f)"))
    assertEquals(lowered("(or (f) $0)"), Some("(let (($tmp-or0 (f))) (if $tmp-or0 $tmp-or0 $0))"))
    assertEquals(lowered("(if x y)"), Some("(if x y (void))"))
    assertEquals(lowered("(cond ((f) => g) (x))"),
      Some("((lambda ($tmp-cond) (if $tmp-cond (g $tmp-cond) ((lambda ($tmp-cond) (if $tmp-cond $tmp-cond (void))) x))) (f))"))
    assertEquals(lowered("(case (f) ((a) x) (() y) (else z))"),
      Some("((lambda ($tmp-case) (if (eqv? $tmp-case 'a) x z)) (f))"))

  val benchmarks = Using.resource(Files.walk(Path.of("benchmarks"))) { paths =>
    paths.iterator.asScala.filter(_.toString.matches(".*\\.(scm|sch)")).toList.sorted
  }

  benchmarks.foreach { path =>
    test(s"parse and pretty-print $path"):
      val ast = SchemeParser.parseFile(path.toString).getOrElse(fail(s"Could not parse $path"))
      val printed = ast.pretty
      val reparsed = SchemeParser(printed).getOrElse(fail(s"Could not reparse the output for $path"))
      assertEquals(reparsed.pretty, printed)
  }

}