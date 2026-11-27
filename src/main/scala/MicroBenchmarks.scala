package llmaam.benchmarks

import llmaam.frontend.anf.{Expr, toANF}
import llmaam.frontend.scm.SchemeParser

// Some micro benchmarks.

private def program(source: String): Expr =
  SchemeParser(source).getOrElse(throw new IllegalArgumentException(s"Invalid example: $source")).toANF

val app1 = program("((lambda (x) (+ x 1)) 2)")

// Minimal non-terminating programs.
val omega = program("((lambda (x) (x x)) (lambda (x) (x x)))")
val omega2 = program("((lambda (x) (x x)) (lambda (y) (y y)))")
val nonterm_let = program("(letrec ((f (lambda (x) (f 1)))) (f 2))")

val stack = program("""
  (let ((id (lambda (z) z)))
    (let* ((x (id 1)) (y (id 2))) x))
""")

val stack2 = program("""
  (let* ((id (lambda (z) z))
         (idid (lambda (w) (id w)))
         (x (idid 1))
         (y (idid 2)))
    x)
""")

val binopid = program("(let ((id (lambda (z) z))) (+ (id 2) (id 2)))")
val binopid2 = program("(let ((id (lambda (z) z))) (+ (id 1) (id 2)))")

val begin1 = program("""
  (let ((id (lambda (x) x)))
    (+ (id 1) (begin (let ((x (id 1))) x) (let ((y (id 2))) y))))
""")

// The final x and y are deliberately unbound.
val beginscope = program("(begin (let ((x 1)) x) (let ((y 2)) y) (+ x y))")
val beginnil = program("(let ((id (lambda (x) x))) (+ (id 1) (begin)))")
val if1 = program("(if (> 1 0) (+ 2 3) (- 4 5))")
val iferr = program("(if (> 1 #t) (+ 2 3) (- 4 5))")

// Loops use recursive Scheme procedures before ANF normalization.
val while1 = program("""
  (let ((i 0) (one 1))
    (let loop () (if (< i 3) (begin (+ 4 5) (loop)) (void))))
""")
val while2 = program("""
  (let ((i 0) (one 1))
    (let loop () (if (< i 3) (begin (set! i (+ i one)) (loop)) (void)))
    3)
""")

val shadowapp = program("((lambda (y) (let ((x 2)) (+ x y))) (let ((x 1)) x))")
val shadowbinop = program("(+ (let ((x 2)) x) (let ((x 1)) x))")
