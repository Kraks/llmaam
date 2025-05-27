package llmaam.benchmarks

import llmaam.syntax.Expr.*

val app1 = App(
  Lam("x", BinOp("+", Var("x"), Lit(1))),
  Lit(2)
)

// A minimal non-terminating program:
// (λx. x x) (λx. x x)
val omega = App(
  Lam("x", App(Var("x"), Var("x"))),
  Lam("x", App(Var("x"), Var("x")))
)

// Same omega combinator but has not renamed:
// (λx. x x) (λy. y y)
val omega2 = App(
  Lam("x", App(Var("x"), Var("x"))),
  Lam("y", App(Var("y"), Var("y")))
)

// letrec f = λx. f(1) in f(2)
val nonterm_let = Letrec("f", Lam("x", App(Var("f"), Lit(1))), App(Var("f"), Lit(2)))

/*
let id = λz. z in
let x = id(1) in
let y = id(2) in
x
*/
val stack = Let("id", Lam("z", Var("z")),
  Let("x", App(Var("id"), Lit(1)),
    Let("y", App(Var("id"), Lit(2)),
      Var("x"))))

/*
let id = λz. z in
let idid = λw. id(w) in
let x = idid(1) in
let y = idid(2) in
x
*/
val stack2 = Let("id", Lam("z", Var("z")),
  Let("idid", Lam("w", App(Var("id"), Var("w"))),
    Let("x", App(Var("idid"), Lit(1)),
      Let("y", App(Var("idid"), Lit(2)),
        Var("x")))))

/*
let id = λz. z in
id(2) + id(2)
*/
val binopid = Let("id", Lam("z", Var("z")),
  BinOp("+", App(Var("id"), Lit(2)), App(Var("id"), Lit(2))))