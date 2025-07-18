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
  def testTranslation(input: String, expected: String): Unit = {
    val e = SchemeParser(input)
      .getOrElse(fail(s"SchemeParser returned None for input: $input"))
    val c = AST2Core(e).toString()
    println(s"Translating: $input to $c")
    assertEquals(c, expected, s"Expected: $expected, but got: $c")
  }

  test("translate Lits"):
    testTranslation("1", "1")
    testTranslation("3.14", "3.14")
    testTranslation("#\\a", "#\\a")
    testTranslation("#t", "true")
    testTranslation("#T", "true")
    testTranslation("#f", "false")
    testTranslation("#F", "false")
    testTranslation("(void)", "(void)")
    testTranslation("3.14+2.7i", "((vector 3.14) 2.7)")

  test("translate Lam"):
    testTranslation("(lambda (z) (- z))", "(λz. (- z))")
    testTranslation(
        "(lambda (x y z) (+ (+ x y) z))",
        "(λx. (λy. (λz. ((x + y) + z))))"
    )

  test("translate App"):
    testTranslation("(+ 1 2)", "(1 + 2)")
    testTranslation(
        "((lambda (x y z) (+ (+ x y) z)) 1 2 3)",
        "((((λx. (λy. (λz. ((x + y) + z)))) 1) 2) 3)"
    )

  test("letrec_to_set"):
    testTranslation(
      "(letrec ([a 3] [b a]) (add a b))",
      "(((λa. (λb. (begin (set! a = 3), (set! b = a), ((add a) b)))) (void)) (void))"
    )

  test("implicit"):
    testTranslation(
      "(add a b) (add a b)",
      "(begin ((add a) b), ((add a) b))"
    )

  test("arith"):
    testTranslation(
        "(+ (- a b) (* (/ 2 3) 4))",
        "((a - b) + ((2 / 3) * 4))"
    )

  test("define_proc"):
    testTranslation(
      "(define (f a b) (+ a b))",
      "(letrec f = (λa. (λb. (a + b))) in (void))"
    )

  test("cond"):
    testTranslation(
      """(cond
          [(positive? -5) (error 1)]
          [(zero? -5) (error 2)]
          [(positive? 5) 'here])""",
      """(if (positive? -5) then (error 1) else (if (zero? -5) then (error 2) else (if (positive? 5) then (quote "here") else (void))))"""
    )

  test("begin_define"):
    testTranslation(
      """(begin
         (display 1)
         (define x 5)
         (display x)
         (define y (+ x 1))
         (+ x y))""",
      "(begin (display 1), (letrec x = 5 in (begin (display x), (letrec y = (x + 1) in (begin (x + y))))))"
    )
}
