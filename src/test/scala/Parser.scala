package llmaam.frontend.scm

import munit.FunSuite
import scala.io.Source

class TestParser extends FunSuite {

  test("pretty-printer"):
    assert(CharLit('a').pretty == "#\\a")

    val e1 = App(Lam(List("x", "y"), App(Var("+"), List(Var("x"), Var("y")))), List(IntLit(1), IntLit(3)))
    assert(e1.pretty == "((lambda (x y) (+ x y)) 1 3)")

    val e2 = If(BoolLit(false), Var("a"), Lam(List("t"), Var("t")))
    assert(e2.pretty == "(if #f a (lambda (t) t))")

  test("quasiquote"):
    assert(SchemeParser("'xxxx") == Some(SSym("xxxx")))
    assert(SchemeParser("'(a 1 ,(a b))") == Some(App(Var("list"),List(SSym("a"), IntLit(1), App(Var("a"),List(Var("b")))))))

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
      "((lambda ($0) (if (eq? $0 3) #t (if (eq? $0 4) #t (if (eq? $0 5) #t (if (eq? $0 ((lambda () 7))) #f (if (eq? $0 6) #f (void))))))) 3)")

    val Some(ast) = SchemeParser("(define (pow a b) (if (eq? b 0) 1 (* a (pow a (- b 1))))) (pow 3 5)")
    assert(ast.desugar.pretty ==
      "(begin (define pow (lambda (a b) (if (eq? b 0) 1 (* a (pow a (- b 1)))))) (pow 3 5))")

    val Some(begin_in_begin) = SchemeParser("(begin (begin a b) c d)")
    assert(begin_in_begin.desugar.pretty == "(begin (begin a b) c d)")

  test("toplas98_boyer"):
    val fileName = "benchmarks/oaam/toplas98/boyer.sch"
    val program = Source.fromFile(fileName).mkString
    assert(SchemeParser(program) != None)

  test("toplas98_nbody"):
    val fileName1 = "benchmarks/oaam/toplas98/nbody.sch"
    val program1 = Source.fromFile(fileName1).mkString
    assert(SchemeParser(program1) != None)
    //val fileName2 = "benchmarks/scm/toplas98/nbody-processed.sch"
    //val program2 = Source.fromFile(fileName2).mkString
    //assert(SchemeParser(program1) == SchemeParser(program2))

  test("toplas98_lattice"):
    val fileName1 = "benchmarks/oaam/toplas98/lattice.scm"
    val program1 = Source.fromFile(fileName1).mkString
    assert(SchemeParser(program1) != None)
    //val fileName2 = "benchmarks/scm/toplas98/lattice-processed.scm"
    //val program2 = Source.fromFile(fileName2).mkString
    //assert(SchemeParser(program1) == SchemeParser(program2))
}