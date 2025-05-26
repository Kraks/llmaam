package llmaam.benchmarks

import llmaam.syntax.Expr.*

val app1 = App(
  Lam("x", BinOp("+", Var("x"), Lit(1))),
  Lit(2)
)