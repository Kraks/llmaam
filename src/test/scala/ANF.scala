package llmaam.frontend.scm

import munit.FunSuite
import llmaam.frontend.counter.reset
import llmaam.frontend.anf.{AExpr as A, CExpr as R, Expr as E, Let as ALet, normalize, toANF}

class TestANF extends FunSuite:
  private def parse(source: String): Expr =
    SchemeParser(source).getOrElse(fail(s"Could not parse $source"))

  // A small concrete interpreter makes scope, mutation, order and recursion tests
  // independent of the particular temporary names chosen by the transformer.
  private class Cell(var value: Any)
  private case class Proc(call: List[Any] => Any)
  private case class Symbol(name: String)
  private type Env = Map[String, Cell]

  private def evaluate(expr: E): Any =
    val primitives: Map[String, Any] = Map(
      "+" -> Proc(xs => xs.map(_.asInstanceOf[Int]).sum),
      "-" -> Proc(xs => xs.head.asInstanceOf[Int] - xs(1).asInstanceOf[Int]),
      "=" -> Proc(xs => xs.head == xs(1)),
      "eqv?" -> Proc(xs => xs.head == xs(1)),
      "list" -> Proc(identity)
    )
    def atom(a: A, env: Env): Any = a match
      case A.IntLit(n) => n
      case A.FloatLit(n) => n
      case A.BoolLit(b) => b
      case A.CharLit(c) => c
      case A.Sym(s) => Symbol(s)
      case A.Void() => ()
      case A.Var(x) => env.getOrElse(x, fail(s"Unbound ANF variable: $x")).value
      case A.Lam(ps, body) => Proc { args =>
        assertEquals(args.length, ps.length)
        run(body, env ++ ps.zip(args.map(v => new Cell(v))))
      }
      case A.RestLam(ps, rest, body) => Proc { args =>
        assert(args.length >= ps.length)
        run(body, env ++ ps.zip(args.take(ps.length).map(v => new Cell(v))) +
          (rest -> new Cell(args.drop(ps.length))))
      }
    def rhs(r: R, env: Env): Any = r match
      case R.Value(a) => atom(a, env)
      case R.App(f, args) => atom(f, env).asInstanceOf[Proc].call(args.map(atom(_, env)))
      case R.If(c, t, f) => run(if atom(c, env) == false then f else t, env)
      case R.SetVar(x, a) => env(x).value = atom(a, env)
    def run(e: E, env: Env): Any = e match
      case a: A => atom(a, env)
      case ALet(x, r, body) => run(body, env + (x -> new Cell(rhs(r, env))))
    run(expr, primitives.map { case (x, v) => x -> new Cell(v) })

  private def check(source: String, expected: Any): Unit =
    assertEquals(evaluate(parse(source).toANF), expected, source)

  private def rhss(expr: E): List[R] =
    def nested(a: A): List[R] = a match
      case A.Lam(_, body) => rhss(body)
      case A.RestLam(_, _, body) => rhss(body)
      case _ => Nil
    expr match
      case a: A => nested(a)
      case ALet(_, rhs, body) =>
        val children = rhs match
          case R.Value(a) => nested(a)
          case R.App(f, args) => (f :: args).flatMap(nested)
          case R.If(_, t, f) => rhss(t) ++ rhss(f)
          case R.SetVar(_, a) => nested(a)
        rhs :: (children ++ rhss(body))

  test("literals remain atomic, including quoted symbols"):
    List(
      IntLit(1) -> A.IntLit(1), FloatLit(1.5) -> A.FloatLit(1.5),
      BoolLit(false) -> A.BoolLit(false), CharLit('a') -> A.CharLit('a'),
      SSym("foo") -> A.Sym("foo"), Void() -> A.Void(), Var("x") -> A.Var("x")
    ).foreach { case (source, expected) => assertEquals(normalize(source), expected) }

  test("nested applications have atomic operands and fresh names"):
    reset
    val source = parse("(f (g 1) (h 2))")
    val expected = ALet("#:x0", R.App(A.Var("g"), List(A.IntLit(1))),
      ALet("#:x1", R.App(A.Var("h"), List(A.IntLit(2))),
        ALet("#:x2", R.App(A.Var("f"), List(A.Var("#:x0"), A.Var("#:x1"))),
          A.Var("#:x2"))))
    assertEquals(normalize(source), expected)
    assertNotEquals(normalize(source), expected)
    reset
    assertEquals(normalize(source), expected)

  test("conditionals normalize tests and branches without duplicating the continuation"):
    val normalized = normalize(parse("(list (if (= 1 1) (+ 2 3) (+ 4 5)) (+ 6 7))"))
    val calls = rhss(normalized).collect { case R.App(A.Var(f), _) => f }
    assertEquals(calls.count(_ == "+"), 3)
    assertEquals(calls.count(_ == "list"), 1)
    assertEquals(evaluate(normalized), List(5, 13))
    check("(if #t 1 (missing))", 1)
    check("(if 0 2 (missing))", 2)

  test("left-to-right operands snapshot mutable variables and the operator"):
    check("(let ((x 1)) (list x (begin (set! x 2) x) x))", List(1, 2, 2))
    check("(let ((f (lambda (x) 1))) (f (begin (set! f (lambda (x) 2)) 0)))", 1)
    check("(let ((x 0)) (list (begin (set! x (+ x 1)) x) (begin (set! x (+ x 1)) x)))", List(1, 2))

  test("begin preserves effects and returns the final value or void"):
    check("(begin)", ())
    check("(begin 1 2 3)", 3)
    check("(let ((x 0)) (begin (set! x 1) (set! x (+ x 1)) x))", 2)
    check("(let ((x 0)) (set! x 1))", ())

  test("fixed, zero and rest argument lambdas keep their calling conventions"):
    check("((lambda () 7))", 7)
    check("((lambda (x y) (+ x y)) 2 3)", 5)
    check("((lambda xs xs) 1 2 3)", List(1, 2, 3))
    check("((lambda (x . xs) (list x xs)) 1 2 3)", List(1, List(2, 3)))
    check("((lambda (x . xs) xs) 1)", Nil)

  test("direct Let ASTs preserve parallel scope and do not capture the continuation"):
    val parallel = Let(List(Bind("x", IntLit(1))),
      Let(List(Bind("x", IntLit(2)), Bind("y", Var("x"))), Var("y")))
    assertEquals(evaluate(normalize(parallel)), 1)
    val scoped = Let(List(Bind("x", IntLit(1))), App(Var("list"), List(
      Let(List(Bind("x", IntLit(2))), Var("x")), Var("x"))))
    assertEquals(evaluate(normalize(scoped)), List(2, 1))

  test("parsed bindings normalize directly without synthetic calls"):
    List("(let ((x 1)) x)", "(letrec ((x 1)) x)", "(let* ((x 1) (y x)) y)").foreach { source =>
      val normalized = normalize(parse(source))
      assertEquals(evaluate(normalized), 1)
      assert(!rhss(normalized).exists { case R.App(_, _) => true; case _ => false }, source)
    }
    check("(let ((x 1)) (let ((x 2) (y x)) y))", 1)
    check("(let ((x 1)) (let* ((x 2) (y x)) y))", 2)
    check("(let () 1 2)", 2)
    check("(letrec () 1 2)", 2)
    check("""(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
                      (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
      (list (even 8) (odd 8)))""", List(true, false))

  test("binding-based parser forms retain their scope and evaluation order"):
    check("(let ((loop 9)) (let loop ((x loop)) x))", 9)
    check("((rec f (lambda (n) (if (= n 0) 0 (+ n (f (- n 1)))))) 4)", 10)
    check("(do ((x 0 (+ x 1)) (y 0 x)) ((= x 3) y))", 2)
    check("(let ((x 0)) (list (or #f (begin (set! x (+ x 1)) x) (missing)) x))", List(1, 1))

  test("recursive bindings and definitions allocate all names before initialization"):
    check("""(define (even n) (if (= n 0) #t (odd (- n 1))))
      (begin (define (odd n) (if (= n 0) #f (even (- n 1)))))
      (list (even 8) (odd 8))""", List(true, false))
    check("(define x 1)", ())
    check("(define x 1) (define x (+ x 1)) x", 2)
    check("(let loop ((n 4)) (if (= n 0) 0 (+ n (loop (- n 1)))))", 10)
    val recursive = Letrec(List(Bind("f", Lam(List("n"),
      If(App(Var("="), List(Var("n"), IntLit(0))), IntLit(0),
        App(Var("f"), List(App(Var("-"), List(Var("n"), IntLit(1))))))))),
      App(Var("f"), List(IntLit(3))))
    assertEquals(evaluate(normalize(recursive)), 0)

  test("internal definitions are local and may capture later definitions"):
    check("""(let ((x 9))
      (list ((lambda () (define (f) x) (define x 3) (f))) x))""", List(3, 9))

  test("cond tests run once and recipients run only for successful tests"):
    check("(cond)", ())
    check("(cond (#f (missing)) (else 4))", 4)
    check("(cond (#f => missing) (else 5))", 5)
    check("(let ((x 0)) (list (cond ((begin (set! x (+ x 1)) x))) x))", List(1, 1))
    check("""(let ((x 0))
      (list (cond ((begin (set! x (+ x 1)) x)
        => (begin (set! x (+ x 1)) (lambda (v) (list v x))))) x))""", List(List(1, 2), 2))

  test("case evaluates its key once and handles empty clauses and fallbacks"):
    check("""(let ((x 0)) (list
      (case (begin (set! x (+ x 1)) x)
        (() (missing)) ((2 3) (missing)) ((0 1) 42) (else (missing))) x))""", List(42, 1))
    check("(case 'a ((b c) 1) ((a d) 2) (else 3))", 2)
    check("(case 7 ((1) 2) (else 3))", 3)
    check("(case 7)", ())

  test("unexpanded syntax is rejected explicitly"):
    val error = intercept[IllegalArgumentException] {
      normalize(parse("(define-syntax f (syntax-rules () ((f) 1)))"))
    }
    assert(error.getMessage.contains("macro expansion"))
