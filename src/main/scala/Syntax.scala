package llmaam.syntax

// Syntax

enum Expr:
  case Lit(n: Int)
  case UnaryOp(op: String, arg: Expr)
  case BinOp(op: String, lhs: Expr, rhs: Expr)
  case Var(x: String)
  case Lam(x: String, body: Expr)
  case App(f: Expr, arg: Expr)
  case Let(x: String, rhs: Expr, body: Expr)
  case Letrec(x: String, rhs: Expr, body: Expr)

// TODO: loop
// TODO: mutable states
// TODO: call/cc