package llmaam.syntax

// Core syntax

val arithBin  = Set("+", "-", "*", "/", "%")
val arithUn   = Set("+", "-")
val relBin    = Set(">", "<", ">=", "<=", "==", "!=")
val logicBin  = Set("&", "|")
val logicUn   = Set("!")

inline def isUnary(op: String): Boolean = arithUn(op) || logicUn(op)
inline def isBinary(op: String): Boolean = arithBin(op) || relBin(op) || logicBin(op)

enum Expr:
  case Lit(n: Int | Boolean | Double | Char | String)
  case Void()
  case UnaryOp(op: String, arg: Expr)
  case BinOp(op: String, lhs: Expr, rhs: Expr)
  case Var(x: String)
  case Lam(x: String, body: Expr)
  case App(f: Expr, arg: Expr)
  case Let(x: String, rhs: Expr, body: Expr)
  case Letrec(x: String, rhs: Expr, body: Expr)
  case Begin(exprs: List[Expr])
  case If(cond: Expr, thn: Expr, els: Expr)
  case While(cond: Expr, body: Expr)
  case SetVar(x: String, rhs: Expr)

  override def toString(): String = this match
    case Lit(n) => n match
      case _: Char => s"#\\$n"
      case _: String => s""""$n""""
      case _ => n.toString
    case Void() => "(void)"
    case UnaryOp(op, arg) => s"($op $arg)"
    case BinOp(op, lhs, rhs) => s"($lhs $op $rhs)"
    case Var(x) => x
    case Lam(x, body) => s"(λ$x. $body)"
    case App(f, arg) => s"($f $arg)"
    case Let(x, rhs, body) => s"(let $x = $rhs in $body)"
    case Letrec(x, rhs, body) => s"(letrec $x = $rhs in $body)"
    case Begin(exprs) => s"(begin ${exprs.mkString(", ")})"
    case If(cond, thn, els) => s"(if $cond then $thn else $els)"
    case While(cond, body) => s"(while $cond do $body)"
    case SetVar(x, rhs) => s"(set! $x = $rhs)"