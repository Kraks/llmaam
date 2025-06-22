package llmaam.aam

import upickle.default.*
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
  case KLetrec(x: String, xa: BAddr, ρ: Env, body: Expr, k: KAddr)
  case KBegin(exprs: List[Expr], ρ: Env, k: KAddr)
  case KIf(thn: Expr, els: Expr, ρ: Env, k: KAddr)
  case KWhileCnd(cond: Expr, body: Expr, ρ: Env, k: KAddr)
  case KWhileBdy(cond: Expr, body: Expr, ρ: Env, k: KAddr)

// The numerical abstract domain can be easily extended to
// non-relational ones, but not obvious to extend to relational ones.

enum Value:
  case Num()
  case Bool()
  case UnitVal()
  case Clo(lam: Expr.Lam, ρ: Env)
  override def toString(): String = this match
    case Num() => "ℤ"
    case Bool() => "𝔹"
    case UnitVal() => "()"
    case Clo(lam, ρ) => s"⟨${lam}, ${ρ}⟩"

import Value.*

val arithBin  = Set("+", "-", "*", "/", "%")
val arithUn   = Set("+", "-")
val relBin    = Set(">", "<", ">=", "<=", "==", "!=")
val logicBin  = Set("&", "|")
val logicUn   = Set("!")

def evalUnaryOp(op: String, v: Value): Option[Value] = (op, v) match
  case (o, Num()) if arithUn(o) => Some(Num())
  case (o, Bool()) if logicUn(o) => Some(Bool())
  case _ => None

def evalBinOp(op: String, v1: Value, v2: Value): Option[Value] =
  (v1, v2) match
    case (Num(), Num()) if arithBin(op) => Some(Num())
    case (Num(), Num()) if relBin(op) => Some(Bool())
    case (Bool(), Bool()) if logicBin(op) => Some(Bool())
    case _ => None

// Addresses

case class BAddr(x: String, instrumentation: List[Any] = List())
case class KAddr(instrumentation: List[Any] = List())

// Environments and stores

type Env = Map[String, BAddr]

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
import State.*

abstract class Analyzer:
  type Label = String

  def tick(t: State): Time
  def allocBind(s: State, x: String, t: Time): BAddr
  /* allocKont can access the current state s,
   * the new control string e1, the new environment ρ1,
   * the new value store σᵥ1, and the instrumentation (``ticked'' history) t.
   */
  def allocKont(s: State, e1: Expr, ρ1: Env, σᵥ1: BStore, t: Time): KAddr

  val transitions: ListBuffer[(State, Label, Set[State])] = ListBuffer()

  def isAtomic(e: Expr): Boolean = e match
    case Lit(_) | Var(_) | Lam(_, _) => true
    case UnaryOp(_, _) | BinOp(_, _, _)
        | App(_, _) | Let(_, _, _) | Letrec(_, _, _)
        | Begin(_) | If(_, _, _) | While(_, _) => false

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
        val α = allocBind(s, x, t)
        val ρ1 = ρ + (x → α)
        val σᵥ1 = σᵥ ⊔ Map(α → Set(v))
        ("app-red", for { kont <- σₖ(k) } yield EState(e, ρ1, σᵥ1, σₖ, kont, t))
      // KUnaryOp expects the result is a Num/Bool
      case VState(v, _, σᵥ, σₖ, KUnaryOp(op, ρ, k), t) =>
        evalUnaryOp(op, v) match
          case Some(res) =>
            ("op1-red", for { kont <- σₖ(k) } yield VState(res, ρ, σᵥ, σₖ, kont, t))
          case None =>
            ("op1-err", ErrState()) // error if the operation is not applicable
      // KBinOpR expects left-hand side is a Num/Bool
      case VState(v, _, σᵥ, σₖ, KBinOpR(op, rhs, ρ, k), t) =>
        ("op2-rhs", EState(rhs, ρ, σᵥ, σₖ, KBinOpL(op, v, k), t))
      // KBinOpL (arithmetic/logic) can be applied to Num/Bool
      case VState(vᵣ, _, σᵥ, σₖ, KBinOpL(op, vₗ, k), t) =>
        evalBinOp(op, vₗ, vᵣ) match
          case Some(res) =>
            ("op2-red", for { kont <- σₖ(k) } yield VState(res, Map(), σᵥ, σₖ, kont, t))
          case None =>
            ("op2-type-error", ErrState())
      case VState(v, _, σᵥ, σₖ, KLet(x, ρ, e, k), t) =>
        val α = allocBind(s, x, t)
        val ρ1 = ρ + (x → α)
        val σᵥ1 = σᵥ ⊔ Map(α → Set(v))
        ("let-body", for { kont <- σₖ(k) } yield EState(e, ρ1, σᵥ1, σₖ, kont, t))
      case VState(v, _, σᵥ, σₖ, KLetrec(x, αₓ, ρ, e, k), t) =>
        val σᵥ1 = σᵥ ⊔ Map(αₓ → Set(v))
        ("letrec-body", for { kont <- σₖ(k) } yield EState(e, ρ, σᵥ1, σₖ, kont, t))
      // KBegin
      case VState(v, _, σᵥ, σₖ, KBegin(Nil, ρ, k), t) =>
        ("begin-done", for { kont <- σₖ(k) } yield VState(v, ρ, σᵥ, σₖ, kont, t))
      case VState(_, _, σᵥ, σₖ, KBegin(exprs, ρ, k), t) =>
        ("begin-next", for { kont <- σₖ(k) } yield EState(Begin(exprs), ρ, σᵥ, σₖ, kont, t))
      // KIf expects the result is a Bool
      case VState(Bool(), _, σᵥ, σₖ, KIf(thn, els, ρ, k), t) =>
        ("if-branch",
          for
            kont   <- σₖ(k)
            branch <- List(thn, els)
          yield EState(branch, ρ, σᵥ, σₖ, kont, t))
      // KWhileCnd expects the result is a Bool
      case VState(Bool(), _, σᵥ, σₖ, KWhileCnd(cond, body, ρ, k), t) =>
        ("while-branch",
          for
            kont <- σₖ(k)
            state <- List(
              VState(UnitVal(), ρ, σᵥ, σₖ, kont, t), // cond false
              EState(body, ρ, σᵥ, σₖ, KWhileBdy(cond, body, ρ, k), t) // cond true
            )
          yield state)
      case VState(_, _, σᵥ, σₖ, KWhileBdy(cond, body, ρ, k), t) =>
        ("while-continue", EState(cond, ρ, σᵥ, σₖ, KWhileCnd(cond, body, ρ, k), t))

  def step(s: State): (Label, Set[State]) =
    val t1 = tick(s)
    s match
      // VState stepping inspects the continuation
      case s@VState(v, ρ, σᵥ, σₖ, k, t) => continue(s)
      // atomic expressions steps to VState
      case EState(Lit(i: Int), ρ, σᵥ, σₖ, k, t) =>
        ("lit-int", VState(Num(), ρ, σᵥ, σₖ, k, t1))
      case EState(Lit(b: Boolean), ρ, σᵥ, σₖ, k, t) =>
        ("lit-bool", VState(Bool(), ρ, σᵥ, σₖ, k, t1))
      case EState(Var(x), ρ, σᵥ, σₖ, k, t) =>
        ρ.get(x) match
          case Some(α) =>
            ("var", σᵥ(α).map { VState(_, ρ, σᵥ, σₖ, k, t1) })
          case None =>
            ("var-unbound", ErrState())
      case EState(Lam(x, e), ρ, σᵥ, σₖ, k, t) =>
        ("lam", VState(Clo(Lam(x, e), ρ), ρ, σᵥ, σₖ, k, t1))
      // push continuation to KStore
      case EState(e@UnaryOp(op, e1), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(s, e1, ρ, σᵥ, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        ("op1-exp", EState(e, ρ, σᵥ, σₖ1, KUnaryOp(op, ρ, α), t1))
      case EState(e@BinOp(op, e1, e2), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(s, e1, ρ, σᵥ, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        ("op2-lhs", EState(e1, ρ, σᵥ, σₖ1, KBinOpR(op, e2, ρ, α), t1))
      case EState(e@App(f, arg), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(s, f, ρ, σᵥ, t1)
        //println(s"Allocating continuation for ${e} at ${α}")
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        ("app-lhs", EState(f, ρ, σᵥ, σₖ1, KArg(arg, ρ, α), t1))
      case EState(e@Let(x, rhs, body), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(s, rhs, ρ, σᵥ, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        ("let-rhs", EState(rhs, ρ, σᵥ, σₖ1, KLet(x, ρ, body, α), t1))
      case EState(e@Letrec(x, rhs, body), ρ, σᵥ, σₖ, k, t) =>
        val αᵥ = allocBind(s, x, t1)
        val ρ1 = ρ + (x → αᵥ)
        val σᵥ1 = σᵥ ⊔ Map(αᵥ → Set())
        val αₖ = allocKont(s, rhs, ρ1, σᵥ1, t1)
        val σₖ1 = σₖ ⊔ Map(αₖ → Set(k))
        ("letrec-rhs", EState(rhs, ρ1, σᵥ1, σₖ1, KLetrec(x, αᵥ, ρ1, body, αₖ), t1))
      case EState(e@Begin(exprs), ρ, σᵥ, σₖ, k, t) =>
        exprs match
          case e1::rest =>
            val α = allocKont(s, e1, ρ, σᵥ, t1)
            val σₖ1 = σₖ ⊔ Map(α → Set(k))
            ("begin-exp", EState(e1, ρ, σᵥ, σₖ1, KBegin(rest, ρ, α), t1))
          case Nil =>
            ("begin-empty", VState(UnitVal(), ρ, σᵥ, σₖ, k, t1))
      case EState(e@If(cond, thn, els), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(s, cond, ρ, σᵥ, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        ("if-cond", EState(cond, ρ, σᵥ, σₖ1, KIf(thn, els, ρ, α), t1))
      case EState(e@While(cond, body), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(s, cond, ρ, σᵥ, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        ("while-cond", EState(cond, ρ, σᵥ, σₖ1, KWhileCnd(cond, body, ρ, α), t1))

  def drive(todo: List[State], seen: Set[State]): Set[State] =
    if (todo.isEmpty) seen
    else
      val s::rest = todo
      if seen(s) then drive(rest, seen)
      else if isDone(s) then drive(rest, seen + s)
      else {
        val (lab, succs) = step(s)
        transitions.append((s, lab, succs))
        drive(succs.toList ++ rest, seen + s)
      }

  def inject(e: Expr): State = EState(e, Map(), Map(), Map(), KHalt(), List())

  def run(e: Expr): Set[State] =
    val initial = inject(e)
    drive(List(initial), Set())

  def dumpGraph(filename: String, printNodeExpr: Boolean = true): Unit =
    import java.io.{File, PrintWriter}
    import java.nio.file.{Files, Paths, Path}
    import scala.sys.process._
    val path = Paths.get("result")
    if (!Files.exists(path)) Files.createDirectories(path)
    val file = new File("result/" + filename)
    val writer = new PrintWriter(file)
    var counter = 0
    val numbering = HashMap[State, Int]()
    def add(s: State): Unit =
      if !numbering.contains(s) then
        numbering(s) = counter
        counter += 1
    writer.println("digraph G {")
    writer.println("""  node [fontname = "Courier New"];""")
    writer.println("""  edge [fontname = "helvetica"];""")
    // print all transitions
    for ((s, lab, succs) <- transitions) {
      add(s)
      for (s2 <- succs) {
        add(s2)
        writer.println(s"""  ${numbering(s)} -> ${numbering(s2)} [label="${lab}"];""")
      }
    }
    // print node labels
    if (printNodeExpr) {
      for ((s, n) <- numbering)
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
    val command = s"dot -Tpdf result/${filename} -o result/${filename.replace(".dot", ".pdf")}"
    println("Executing command: " + command)
    command.!