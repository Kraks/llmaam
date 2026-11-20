package llmaam.frontend.scm

import munit.FunSuite
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

class TestParser extends FunSuite {
  test("pretty-printer"):
    assert(CharLit('a').pretty == "#\\a")

    val e1 = App(Lam(List("x", "y"), App(Var("+"), List(Var("x"), Var("y")))), List(IntLit(1), IntLit(3)))
    assert(e1.pretty == "((lambda (x y) (+ x y)) 1 3)")

    val e2 = If(BoolLit(false), Var("a"), Lam(List("t"), Var("t")))
    assert(e2.pretty == "(if #f a (lambda (t) t))")

  test("quasiquote"):
    assert(SchemeParser("'xxxx") == Some(SSym("xxxx")))
    assert(SchemeParser("`(a 1 ,(a b))") == Some(App(Var("list"),List(SSym("a"), IntLit(1), App(Var("a"),List(Var("b")))))))

  test("letrec_to_set"):
    val actual = SchemeParser("(letrec ([a 3] [b a]) (add a b))")
    val expected = Some(
      App(
        Lam(List("a", "b"),
          Begin(
            List(
              SetVar("a",IntLit(3)),
              SetVar("b",Var("a")),
              App(Var("add"),List(Var("a"), Var("b")))
            )
          )
        ),
      List(Void(), Void())))
    assert(actual == expected)

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
    val proc = Lrc(List(Bind("f", Lam(List("x"), Var("x")))), Var("f")).toApp
    assertEquals(SchemeParser("(let f ((x f)) x)"), Some(App(proc, List(Var("f")))))

  test("do uses old bindings in every step and retains an omitted step"):
    val call = App(Var("#:do"), List(Var("y"), Var("y")))
    val loop = Lam(List("x", "y"), If(Var("done"), Var("x"), Begin(List(Var("work"), call))))
    val proc = Lrc(List(Bind("#:do", loop)), Var("#:do")).toApp
    assertEquals(SchemeParser("(do ((x 0 y) (y 1)) (done x) work)"), Some(App(proc, List(IntLit(0), IntLit(1)))))

  test("conditional forms preserve values and evaluate tests once"):
    def lowered(s: String) = SchemeParser(s).map(_.desugar.pretty)
    assertEquals(lowered("(and)"), Some("#t"))
    assertEquals(lowered("(or)"), Some("#f"))
    assertEquals(lowered("(and x y)"), Some("(if x y #f)"))
    assertEquals(lowered("(or (f) $0)"), Some("((lambda (#:or) (if #:or #:or $0)) (f))"))
    assertEquals(lowered("(if x y)"), Some("(if x y (void))"))
    assertEquals(lowered("(cond ((f) => g) (x))"),
      Some("((lambda (#:cond) (if #:cond (g #:cond) ((lambda (#:cond) (if #:cond #:cond (void))) x))) (f))"))
    assertEquals(lowered("(case (f) ((a) x) (() y) (else z))"),
      Some("((lambda (#:case) (if (eqv? #:case 'a) x z)) (f))"))

  val benchmarks = Using.resource(Files.walk(Path.of("benchmarks"))) { paths =>
    paths.iterator.asScala.filter(_.toString.matches(".*\\.(scm|sch)")).toList.sorted
  }

  benchmarks.foreach { path =>
    test(s"parse $path"):
      assert(SchemeParser.parseFile(path.toString).isDefined)
  }

}

class TestDesugar extends FunSuite {
  test("desugar"):
    assert(IntLit(1).desugar == IntLit(1))

    assert((Begin(List(Define("x", IntLit(2)), SetVar("x", IntLit(3)), Var("x")))).desugar.pretty ==
      "(begin (define x 2) (set! x 3) x)")

    assert(Cond(List(
        CondBr(
          App(Var("positive?"),List(IntLit(-5))),
          App(Var("error"),List())),
        CondBr(App(Var("zero?"),List(IntLit(-5))),App(Var("error"),List())),
        CondBr(App(Var("positive?"),List(IntLit(5))),SSym("here")))).desugar.pretty ==
        "(if (positive? -5) (error) (if (zero? -5) (error) (if (positive? 5) 'here (void))))")

    assert(Case(IntLit(3), List(
        CaseBranch(List(IntLit(3), IntLit(4), IntLit(5)), BoolLit(true)),
        CaseBranch(List(App(Lam(List(), IntLit(7)), List()), IntLit(6)), BoolLit(false)))).desugar.pretty ==
      "((lambda (#:case) (if (eqv? #:case 3) #t (if (eqv? #:case 4) #t (if (eqv? #:case 5) #t (if (eqv? #:case ((lambda () 7))) #f (if (eqv? #:case 6) #f (void))))))) 3)")

    val Some(ast) = SchemeParser("(define (pow a b) (if (eq? b 0) 1 (* a (pow a (- b 1))))) (pow 3 5)")
    assert(ast.desugar.pretty ==
      "(begin (define pow (lambda (a b) (if (eq? b 0) 1 (* a (pow a (- b 1)))))) (pow 3 5))")

    val Some(begin_in_begin) = SchemeParser("(begin (begin a b) c d)")
    assert(begin_in_begin.desugar.pretty == "(begin (begin a b) c d)")
}