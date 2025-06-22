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

/*
let id = λz. z in
id(1) + id(2)
*/
val binopid2 = Let("id", Lam("z", Var("z")),
  BinOp("+", App(Var("id"), Lit(1)), App(Var("id"), Lit(2))))

/*
let id = λx. x in
id(1) + begin
  let x = id(1) in x
  let y = id(2) in y
end
 */
val begin1 = Let("id", Lam("x", Var("x")),
  BinOp("+",
    App(Var("id"), Lit(1)),
    Begin(List(
      Let("x", App(Var("id"), Lit(1)), Var("x")),
      Let("y", App(Var("id"), Lit(2)), Var("y"))
    ))
  )
)

/*
begin
  let x = 1 in x
  let y = 2 in y
  x + y
end
*/
val beginscope = Begin(List(
  Let("x", Lit(1), Var("x")),
  Let("y", Lit(2), Var("y")),
  BinOp("+", Var("x"), Var("y"))
))

/*
let id = λx. x in
id(1) + begin
end
 */
val beginnil = Let("id", Lam("x", Var("x")),
  BinOp("+",
    App(Var("id"), Lit(1)),
    Begin(List())
  )
)

/*
if 1 > 0 then 2 + 3 else 4 - 5
 */
val if1 = If(
  BinOp(">", Lit(1), Lit(0)),
  BinOp("+", Lit(2), Lit(3)),
  BinOp("-", Lit(4), Lit(5))
)

val iferr = If(
  BinOp(">", Lit(1), Lit(true)), // 1 cannot be compared to true
  BinOp("+", Lit(2), Lit(3)),
  BinOp("-", Lit(4), Lit(5))
)

/*
let i = 0 in
while i < 3 do
  4 + 5
 */
val while1 = Let("i", Lit(0),
  Let("one", Lit(1),
    While(
      BinOp("<", Var("i"), Lit(3)),
      BinOp("+", Lit(4), Lit(5))
    )
  )
)

/*
(λy. let x = 2 in x + y) (let x = 1 in x)
 */
val shadowapp = App(
  Lam("y", Let("x", Lit(2), BinOp("+", Var("x"), Var("y")))),
  Let("x", Lit(1), Var("x"))
)

/*
(let x = 2 in x) + (let x = 1 in x)
 */
val shadowbinop = BinOp(
  "+",
  Let("x", Lit(2), Var("x")),
  Let("x", Lit(1), Var("x"))
)