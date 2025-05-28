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

  override def toString(): String = this match
    case Lit(n) => n.toString
    case UnaryOp(op, arg) => s"($op $arg)"
    case BinOp(op, lhs, rhs) => s"($lhs $op $rhs)"
    case Var(x) => x
    case Lam(x, body) => s"(λ$x. $body)"
    case App(f, arg) => s"($f $arg)"
    case Let(x, rhs, body) => s"(let $x = $rhs in $body)"
    case Letrec(x, rhs, body) => s"(letrec $x = $rhs in $body)"

// TODO: conditionals, boolean
// TODO: loop
// TODO: mutable states
// TODO: call/cc