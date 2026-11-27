package llmaam.aam

import munit.FunSuite
import llmaam.frontend.anf.{Expr, AExpr as A, CExpr as C, Let, toANF}
import llmaam.frontend.scm.SchemeParser
import State.*
import AbsVal.*
import Kont.*

// AI-generated tests

class TestANFAAM extends FunSuite:
  private def program(source: String): Expr =
    SchemeParser(source).getOrElse(fail(s"Could not parse $source")).toANF

  private def analyzer(): Analyzer = new Analyzer with ZeroCFA with P4FContAlloc

  private def answers(states: Set[State]): Set[AbsVal] = states.collect {
    case VState(value, _, _, _, KHalt(), _) => value
  }

  private def check(source: String, expected: Set[AbsVal]): Unit =
    val states = analyzer().run(program(source))
    assertEquals(answers(states), expected, source)
    assert(!states.contains(ErrState()), source)

  private def checkError(source: String): Unit =
    val states = analyzer().run(program(source))
    assert(states.contains(ErrState()), source)
    assertEquals(answers(states), Set.empty[AbsVal], source)

  test("terminal atoms are evaluated, including unbound variables"):
    check("1", Set(Num()))
    check("1.5", Set(Num()))
    check("#t", Set(Bool()))
    check("#\\a", Set(CharVal()))
    check("'symbol", Set(SymVal()))
    check("(void)", Set(UnitVal()))
    check("(begin)", Set(UnitVal()))
    check("+", Set(Prim("+")))
    checkError("unbound")

  test("applications preserve zero, multiple and rest parameters"):
    check("((lambda () 7))", Set(Num()))
    check("((lambda (x y) y) 1 #t)", Set(Bool()))
    check("((lambda (x y z) (+ x y z)) 1 2 3)", Set(Num()))
    check("((lambda args args))", Set(ListVal()))
    check("((lambda (x . rest) rest) 1 2 #t)", Set(ListVal()))
    check("((lambda (x . rest) x) 1 #t)", Set(Num()))
    checkError("((lambda (x) x))")
    checkError("((lambda () 1) 2)")
    checkError("((lambda (x y . rest) x) 1)")
    checkError("(1 2)")

  test("primitives are first-class and lexical bindings shadow their names"):
    check("(let ((f +)) (f 1 2 3))", Set(Num()))
    check("((lambda (f) (f 1 2)) +)", Set(Num()))
    check("(let ((+ (lambda (x y) #t))) (+ 1 2))", Set(Bool()))
    check("(list 1 #t)", Set(ListVal()))
    check("(list)", Set(ListVal()))
    check("(input)", Set(Num()))
    check("(+)", Set(Num()))
    check("(*)", Set(Num()))
    check("(- 1)", Set(Num()))
    check("(eqv? 'a 'b)", Set(Bool()))
    check("(not 1)", Set(Bool()))
    checkError("(+ 1 #t)")
    checkError("(-)")
    checkError("(let ((+ 1)) (+ 2 3))")
    checkError("(missing 1)")

  test("application explores all abstract operand combinations, including errors"):
    val machine = analyzer()
    val x = BAddr("x")
    val expr = Let("r", C.App(A.Var("+"), List(A.Var("x"), A.IntLit(1))), A.Var("r"))
    val initial = EState(expr, Map("x" -> x), Map(x -> Set(Num(), Bool())), Map.empty, KHalt(), Nil)
    val states = machine.drive(List(initial), Set.empty)
    assertEquals(answers(states), Set[AbsVal](Num()))
    assert(states.contains(ErrState()))

  test("conditionals evaluate branches under a let continuation and use Scheme truthiness"):
    check("(if (= 1 2) 1 #t)", Set(Num(), Bool()))
    check("(if 0 1 (missing))", Set(Num()))
    check("(if 'a 1 (missing))", Set(Num()))
    check("(if (list) 1 (missing))", Set(Num()))
    check("(if (lambda () 1) 2 (missing))", Set(Num()))
    check("(+ (if (= 1 2) 3 4) 5)", Set(Num()))
    checkError("(if unknown 1 2)")

  test("closures share store locations and assignments use weak updates"):
    check("(let ((x 1)) (set! x #t) x)", Set(Num(), Bool()))
    check("(let ((x 1)) (set! x 2))", Set(UnitVal()))
    check("""(let ((x 1))
      (let ((read (lambda () x))) (set! x #t) (read)))""", Set(Num(), Bool()))
    checkError("(set! missing 1)")
    checkError("(let ((x 1)) (set! x missing))")

  test("recursive definitions and letrec remain analyzable"):
    val sources = List(
      "(define (f) (g)) (define (g) 1) (f)",
      "(letrec ((f (lambda () (g))) (g (lambda () 1))) (f))",
      "(let loop ((n 2)) (if (= n 0) 1 (loop (- n 1))))"
    )
    sources.foreach { source =>
      val states = analyzer().run(program(source))
      assertEquals(answers(states), Set[AbsVal](Num()), source)
      // Weak updates may retain the Void placeholders alongside initialized closures.
    }

  test("returning from a call restores the caller environment"):
    check("(let ((x 1)) ((lambda (x) x) #t) x)", Set(Num()))
    check("(let ((x 1)) (let ((f (lambda () x))) (let ((x #t)) (f))))", Set(Num()))
    check("(let ((x 1)) (let ((x #t) (y x)) y))", Set(Num()))
    check("(((lambda (x) (lambda () x)) 1))", Set(Num()))

  test("continuation allocation receives the callee environment and parameter store"):
    val calls = scala.collection.mutable.ListBuffer.empty[(Expr, Env, BStore)]
    val machine = new Analyzer with ZeroCFA:
      def allocKont(s: State, target: Expr, env: Env, store: BStore, t: Time): KAddr =
        calls.append((target, env, store))
        KAddr(List(target, env))
    val target = A.Var("arg")
    val expr = Let("result", C.App(A.Lam(List("arg"), target), List(A.IntLit(1))), A.Var("result"))
    assertEquals(answers(machine.run(expr)), Set[AbsVal](Num()))
    assertEquals(calls.toList, List((target, Map("arg" -> BAddr("arg")), Map(BAddr("arg") -> Set[AbsVal](Num())))))

  test("k-CFA ticks the enclosing let call site only"):
    val machine = new Analyzer with KCFA(1) with P4FContAlloc
    val expr = program("(+ 1 2)")
    assertEquals(machine.tick(machine.inject(expr)), List(expr))
    val value = VState(Num(), Map.empty, Map.empty, Map.empty, KHalt(), List(expr))
    assertEquals(machine.tick(value), List(expr))
    assertEquals(machine.tick(machine.inject(A.IntLit(1))), Nil)

  test("allocation strategies all run the ANF higher-order example"):
    val expr = program("(let* ((id (lambda (x) x)) (call (lambda (y) (id y)))) (call 1))")
    val machines = List(
      new Analyzer with ZeroCFA with SrcContAlloc,
      new Analyzer with KCFA(1) with TgtContAlloc,
      new Analyzer with KCFA(1) with SrcTgtContAlloc,
      new Analyzer with KCFA(1) with P4FContAlloc,
      new Analyzer with ZeroCFA with AACContAlloc
    )
    machines.foreach { machine =>
      val states = machine.run(expr)
      assertEquals(answers(states), Set[AbsVal](Num()))
      assert(!states.contains(ErrState()))
    }
