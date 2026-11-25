package llmaam.frontend.scm

import munit.FunSuite

import llmaam.syntax.Expr as C // Core Syntax

class Playground extends FunSuite{
  test("multi-define"):
    val input ="""
    (define (add x y) (+ x y))
    (define (sub x y) (- x y))
    (add 1 (sub 5 3))
    ;;(letrec ([add (lambda (x y) (+ x y))] [sub (lambda (x y) (- x y))])
    ;;  (begin
    ;;    ((add 1) ((sub 5) 3))))
    """
    val e = SchemeParser(input)
      .getOrElse(fail(s"SchemeParser returned None for input: $input"))
    val c = AST2Core(e).toString()
    println(s"""Translating: $input
    source:    ${e}
    desugared: ${e.desugar}
    core:      $c""")
    //(letrec add = (λx. (λy. (x + y))) in (letrec sub = (λx. (λy. (x - y))) in (begin ((add 1) ((sub 5) 3)))))
}

class TestAST2Core extends FunSuite {
  import C.*

  private def testTranslation(input: String, expected: C): Unit =
    val parsed = SchemeParser(input).getOrElse(fail(s"Could not parse $input"))
    assertEquals(AST2Core(parsed), expected, input)

  private def prim(op: String, args: C*): C = PrimOp(op, args.toList)

  private val sumXYZ = Lam("x", Lam("y", Lam("z", prim("+", prim("+", Var("x"), Var("y")), Var("z")))))
  private val addAB = App(App(Var("add"), Var("a")), Var("b"))

  test("translate Lits"):
    testTranslation("1", Lit(1))
    testTranslation("3.14", Lit(3.14))
    testTranslation("#\\a", Lit('a'))
    testTranslation("#t", Lit(true))
    testTranslation("#T", Lit(true))
    testTranslation("#f", Lit(false))
    testTranslation("#F", Lit(false))
    testTranslation("(void)", Void())
    testTranslation("3.14+2.7i", App(App(Var("vector"), Lit(3.14)), Lit(2.7)))

  test("translate Lam"):
    testTranslation("(lambda (z) (- z))", Lam("z", prim("-", Var("z"))))
    testTranslation("(lambda (x y z) (+ (+ x y) z))", sumXYZ)

  test("translate App"):
    testTranslation("(+ 1 2)", prim("+", Lit(1), Lit(2)))
    testTranslation(
      "((lambda (x y z) (+ (+ x y) z)) 1 2 3)",
      App(App(App(sumXYZ, Lit(1)), Lit(2)), Lit(3))
    )

  test("translate explicit letrec"):
    testTranslation(
      "(letrec ([a 3] [b a]) (add a b))",
      Letrec("a", Lit(3), Letrec("b", Var("a"), addAB))
    )

  test("implicit"):
    testTranslation("(add a b) (add a b)", Begin(List(addAB, addAB)))

  test("arith"):
    testTranslation(
      "(+ (- a b) (* (/ 2 3) 4))",
      prim("+", prim("-", Var("a"), Var("b")), prim("*", prim("/", Lit(2), Lit(3)), Lit(4)))
    )

  test("define_proc"):
    testTranslation(
      "(define (f a b) (+ a b))",
      Letrec("f", Lam("a", Lam("b", prim("+", Var("a"), Var("b")))), Void())
    )

  test("cond"):
    val last = If(App(Var("positive?"), Lit(5)), prim("quote", Lit("here")), Void())
    val second = If(App(Var("zero?"), Lit(-5)), App(Var("error"), Lit(2)), last)
    testTranslation(
      """(cond
          [(positive? -5) (error 1)]
          [(zero? -5) (error 2)]
          [(positive? 5) 'here])""",
      If(App(Var("positive?"), Lit(-5)), App(Var("error"), Lit(1)), second)
    )

  test("begin_define"):
    val x = Var("x")
    val bindY = Letrec("y", prim("+", x, Lit(1)), Begin(List(prim("+", x, Var("y")))))
    val bindX = Letrec("x", Lit(5), Begin(List(App(Var("display"), x), bindY)))
    testTranslation(
      """(begin
         (display 1)
         (define x 5)
         (display x)
         (define y (+ x 1))
         (+ x y))""",
      Begin(List(App(Var("display"), Lit(1)), bindX))
    )
}
