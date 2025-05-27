package llmaam.aam

import scala.collection.mutable.{ListBuffer, HashMap}

import llmaam.syntax.*
import Expr.*

// Frames and continuations

enum Kont:
  case KHalt()
  case KUnaryOp(op: String, ρ: Env, k: KAddr)
  case KBinOpR(op: String, rhs: Expr, ρ: Env, k: KAddr)
  case KBinOpL(op: String, lhs: Value, k: KAddr)
  case KArg(e: Expr, ρ: Env, k: KAddr)
  case KFun(lam: Expr.Lam, ρ: Env, k: KAddr)
  case KLet(x: String, ρ: Env, body: Expr, k: KAddr)
  case KLetrec(x: String, ρ: Env, body: Expr, k: KAddr)

// Addresses

case class BAddr(x: String, instrumentation: Time = List())
case class KAddr(e: Expr, instrumentation: Time = List())

// Environments and stores

type Env = Map[String, BAddr]

// The numerical abstract domain can be easily extended to
// non-relational ones, but not obvious to extend to relational ones.

enum Value:
  case Num()
  case Clo(lam: Expr.Lam, ρ: Env)

type BStore = Map[BAddr, Set[Value]]
type KStore = Map[KAddr, Set[Kont]]
type Time = List[Expr]

enum State:
  case EState(e: Expr, ρ: Env, σᵥ: BStore, σₖ: KStore, k: Kont, t: Time)
  case VState(v: Value, ρ: Env, σᵥ: BStore, σₖ: KStore, k: Kont, t: Time)
  case ErrState()

given Conversion[State, Set[State]] with
  def apply(s: State): Set[State] = Set(s)

import Kont.*
import Value.*
import State.*

abstract class Analyzer:
  type Label = String

  def tick(t: State): Time
  def allocBind(x: String, t: Time): BAddr
  def allocKont(e: Expr, t: Time): KAddr

  val order: ListBuffer[(State, (Label, Set[State]))] = ListBuffer()

  def isAtomic(e: Expr): Boolean = e match
    case Lit(_) | Var(_) | Lam(_, _) => true
    case UnaryOp(_, _) | BinOp(_, _, _)
        | App(_, _) | Let(_, _, _) | Letrec(_, _, _) => false

  def isDone(s: State): Boolean = s match
    case EState(e, _, _, _, KHalt(), _) if isAtomic(e) => true
    case VState(e, _, _, _, KHalt(), _) => true
    case ErrState() => true
    case _ => false

  def continue(s: VState): (Label, Set[State]) =
    s match
      // KArg expects a closure value
      case VState(Clo(lam, ρ1), _, σᵥ, σₖ, KArg(e, ρ2, k), t) =>
        ("app-arg", EState(e, ρ2, σᵥ, σₖ, KFun(lam, ρ1, k), t))
      // KFun (beta) can be applied to any argument value
      case VState(v, _, σᵥ, σₖ, KFun(Lam(x, e), ρ, k), t) =>
        val α = allocBind(x, t)
        val ρ1 = ρ + (x → α)
        val σᵥ1 = σᵥ ⊔ Map(α → Set(v))
        ("app-red", for { kont <- σₖ(k) } yield EState(e, ρ1, σᵥ1, σₖ, kont, t))
      // KUnaryOp expects the result is a Num
      case VState(Num(), _, σᵥ, σₖ, KUnaryOp(op, ρ, k), t) =>
        ("op1-red", for { kont <- σₖ(k) } yield VState(Num(), ρ, σᵥ, σₖ, kont, t))
      // KBinOpR expects left-hand side is a Num
      case VState(Num(), _, σᵥ, σₖ, KBinOpR(op, rhs, ρ, k), t) =>
        ("op2-rhs", EState(rhs, ρ, σᵥ, σₖ, KBinOpL(op, Num(), k), t))
      // KBinOpL (arithmetic) can be applied to number and number
      case VState(Num(), _, σᵥ, σₖ, KBinOpL(op, Num(), k), t) =>
        ("op2-red", for { kont <- σₖ(k) } yield VState(Num(), Map(), σᵥ, σₖ, kont, t))
      case VState(v, _, σᵥ, σₖ, KLet(x, ρ, e, k), t) =>
        val α = allocBind(x, t)
        val ρ1 = ρ + (x → α)
        val σᵥ1 = σᵥ ⊔ Map(α → Set(v))
        ("let", for { kont <- σₖ(k) } yield EState(e, ρ1, σᵥ1, σₖ, kont, t))
      case VState(v, _, σᵥ, σₖ, KLetrec(x, ρ, e, k), t) =>
        val α = allocBind(x, t)
        val ρ1 = ρ + (x → α)
        val σᵥ1 = σᵥ ⊔ Map(α → Set(v))
        ("letrec", for { kont <- σₖ(k) } yield EState(e, ρ1, σᵥ1, σₖ, kont, t))

  def step(s: State): (Label, Set[State]) =
    val t1 = tick(s)
    s match
      // VState stepping inspects the continuation
      case s@VState(v, ρ, σᵥ, σₖ, k, t) => continue(s)
      // atomic expressions steps to VState
      case EState(Lit(_), ρ, σᵥ, σₖ, k, t) =>
        ("lit", VState(Num(), ρ, σᵥ, σₖ, k, t1))
      case EState(Var(x), ρ, σᵥ, σₖ, k, t) =>
        ("var", σᵥ(ρ(x)).map { VState(_, ρ, σᵥ, σₖ, k, t1) })
      case EState(Lam(x, e), ρ, σᵥ, σₖ, k, t) =>
        ("lam", VState(Clo(Lam(x, e), ρ), ρ, σᵥ, σₖ, k, t1))
      // push continuation to KStore
      case EState(e@UnaryOp(op, e1), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(e, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        ("op1-exp", EState(e, ρ, σᵥ, σₖ1, KUnaryOp(op, ρ, α), t1))
      case EState(e@BinOp(op, e1, e2), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(e, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        ("op2-lhs", EState(e1, ρ, σᵥ, σₖ1, KBinOpR(op, e2, ρ, α), t1))
      case EState(e@App(f, arg), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(e, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        ("app-lhs", EState(f, ρ, σᵥ, σₖ1, KArg(arg, ρ, α), t1))
      case EState(e@Let(x, rhs, body), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(e, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        ("let-rhs", EState(rhs, ρ, σᵥ, σₖ1, KLet(x, ρ, body, α), t1))
      case EState(e@Letrec(x, rhs, body), ρ, σᵥ, σₖ, k, t) =>
        val αb = allocBind(x, t1)
        val αk = allocKont(e, t1)
        val ρ1 = ρ + (x → αb)
        val σᵥ1 = σᵥ ⊔ Map(αb → Set())
        val σₖ1 = σₖ ⊔ Map(αk → Set(k))
        ("letrec-rhs", EState(rhs, ρ1, σᵥ1, σₖ1, KLetrec(x, ρ, body, αk), t1))

  def drive(todo: List[State], seen: Set[State]): Set[State] =
    if (todo.isEmpty) seen
    else
      val s::rest = todo
      if seen(s) then drive(rest, seen)
      else if isDone(s) then drive(rest, seen + s)
      else {
        val (lab, succs) = step(s)
        order.append(s -> (lab, succs))
        drive(succs.toList ++ rest, seen + s)
      }

  def inject(e: Expr): State = EState(e, Map(), Map(), Map(), KHalt(), List())

  def run(e: Expr): Set[State] =
    val initial = inject(e)
    drive(List(initial), Set())

  def dumpGraph(filename: String): Unit =
    import java.io.{File, PrintWriter}
    import scala.sys.process._
    val file = new File(filename)
    val writer = new PrintWriter(file)
    var counter = 0
    val numbering = HashMap[State, Int]()
    def add(s: State): Unit =
      if !numbering.contains(s) then
        numbering(s) = counter
        counter += 1
    writer.println("digraph G {")
    // print all transitions
    for ((s, (lab, succs)) <- order) {
      add(s)
      for (s2 <- succs) {
        add(s2)
        writer.println(s"""  ${numbering(s)} -> ${numbering(s2)} [label="${lab}"];""")
      }
    }
    // print node labels
    for ((s, n) <- numbering) {
      s match
        case EState(e, ρ, σᵥ, σₖ, k, t) =>
          writer.println(s"""  ${n} [label="${n}|EState(${e})"];""")
        case VState(v, ρ, σᵥ, σₖ, k, t) =>
          writer.println(s"""  ${n} [label="${n}|VState(${v})"];""")
        case ErrState() =>
          writer.println(s"""  ${n} [label="${n}|ErrState()"];""")
    }
    writer.println("}")
    writer.close()
    val command = s"dot -Tpng ${filename} -o ${filename.replace(".dot", ".png")}"
    println("Executing command: " + command)
    command.!

class Analyzer0CFA extends Analyzer:
  def tick(t: State): Time = List()
  def allocBind(x: String, t: Time): BAddr = BAddr(x, List())
  def allocKont(e: Expr, t: Time): KAddr = KAddr(e, List())
