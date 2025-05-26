package llmaam.aam

import scala.collection.mutable.HashMap

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

trait Lattice[T]:
  def bot: T
  def top: T
  extension (x: T)
    def ⊑(y: T): Boolean
    def ⊔(y: T): T
    def ⊓(y: T): T

given ProductLattice[A: Lattice, B: Lattice]: Lattice[(A, B)] with
  val la: Lattice[A] = summon[Lattice[A]]
  val lb: Lattice[B] = summon[Lattice[B]]
  def bot: (A, B) = (la.bot, lb.bot)
  def top: (A, B) = (la.top, lb.top)
  extension (x: (A, B))
    def ⊑(y: (A, B)): Boolean = x._1 ⊑ y._1 && x._2 ⊑ y._2
    def ⊔(y: (A, B)): (A, B) = (x._1 ⊔ y._1, x._2 ⊔ y._2)
    def ⊓(y: (A, B)): (A, B) = (x._1 ⊓ y._1, x._2 ⊓ y._2)

given MapLattice[K, V: Lattice]: Lattice[Map[K, V]] with
  val lv: Lattice[V] = summon[Lattice[V]]
  def bot: Map[K, V] = Map[K, V]()
  def top: Map[K, V] = throw new RuntimeException("No representation of top map")
  extension (m1: Map[K, V])
    def ⊑(m2: Map[K, V]): Boolean =
      m1.forall { case (k, v) => v ⊑ m2.getOrElse(k, lv.bot) }
    def ⊔(m2: Map[K, V]): Map[K, V] =
      m1.foldLeft(m2) { case (m, (k, v)) => m + (k -> v ⊔ m.getOrElse(k, lv.bot)) }
    def ⊓(m2: Map[K, V]): Map[K, V] =
      m1.keySet.intersect(m2.keySet).foldLeft(Map[K,V]()) {
        case (m, k) => m + (k -> m1(k) ⊓ m2(k))
      }

given SetLattice[T]: Lattice[Set[T]] with
  def bot: Set[T] = Set[T]()
  def top: Set[T] = throw new RuntimeException("No representation of top set")
  extension (s1: Set[T])
    def ⊑(s2: Set[T]): Boolean = s1.subsetOf(s2)
    def ⊔(s2: Set[T]): Set[T] = s1 ++ s2
    def ⊓(s2: Set[T]): Set[T] = s1.intersect(s2)

abstract class Analyzer:
  def tick(t: State): Time
  def allocBind(x: String, t: Time): BAddr
  def allocKont(e: Expr, t: Time): KAddr

  val order: HashMap[State, Set[State]] = HashMap()

  def isAtomic(e: Expr): Boolean = e match
    case Lit(_) | Var(_) | Lam(_, _) => true
    case UnaryOp(_, _) | BinOp(_, _, _)
        | App(_, _) | Let(_, _, _) | Letrec(_, _, _) => false

  def isDone(s: State): Boolean = s match
    case EState(e, _, _, _, KHalt(), _) if isAtomic(e) => true
    case VState(e, _, _, _, KHalt(), _) => true
    case ErrState() => true
    case _ => false

  def continue(s: VState): Set[State] =
    s match
      // KArg expects a closure value
      case VState(Clo(lam, ρ1), _, σᵥ, σₖ, KArg(e, ρ2, k), t) =>
        EState(e, ρ2, σᵥ, σₖ, KFun(lam, ρ1, k), t)
      // KFun (beta) can be applied to any argument value
      case VState(v, _, σᵥ, σₖ, KFun(Lam(x, e), ρ, k), t) =>
        val α = allocBind(x, t)
        val ρ1 = ρ + (x → α)
        val σᵥ1 = σᵥ ⊔ Map(α → Set(v))
        for { kont <- σₖ(k) } yield EState(e, ρ1, σᵥ1, σₖ, kont, t)
      // KUnaryOp expects the result is a Num
      case VState(Num(), _, σᵥ, σₖ, KUnaryOp(op, ρ, k), t) =>
        for { kont <- σₖ(k) } yield VState(Num(), ρ, σᵥ, σₖ, kont, t)
      // KBinOpR expects left-hand side is a Num
      case VState(Num(), _, σᵥ, σₖ, KBinOpR(op, rhs, ρ, k), t) =>
        EState(rhs, ρ, σᵥ, σₖ, KBinOpL(op, Num(), k), t)
      // KBinOpL (arithmetic) can be applied to number and number
      case VState(Num(), _, σᵥ, σₖ, KBinOpL(op, Num(), k), t) =>
        for { kont <- σₖ(k) } yield VState(Num(), Map(), σᵥ, σₖ, kont, t)
      case VState(v, _, σᵥ, σₖ, KLet(x, ρ, e, k), t) =>
        val α = allocBind(x, t)
        val ρ1 = ρ + (x → α)
        val σᵥ1 = σᵥ ⊔ Map(α → Set(v))
        for { kont <- σₖ(k) } yield EState(e, ρ1, σᵥ1, σₖ, kont, t)
      case VState(v, _, σᵥ, σₖ, KLetrec(x, ρ, e, k), t) =>
        val α = allocBind(x, t)
        val ρ1 = ρ + (x → α)
        val σᵥ1 = σᵥ ⊔ Map(α → Set(v))
        for { kont <- σₖ(k) } yield EState(e, ρ1, σᵥ1, σₖ, kont, t)

  def step(s: State): Set[State] =
    val t1 = tick(s)
    s match
      // VState stepping inspects the continuation
      case s@VState(v, ρ, σᵥ, σₖ, k, t) => continue(s)
      // atomic expressions steps to VState
      case EState(Lit(_), ρ, σᵥ, σₖ, k, t) =>
        VState(Num(), ρ, σᵥ, σₖ, k, t1)
      case EState(Var(x), ρ, σᵥ, σₖ, k, t) =>
        σᵥ(ρ(x)).map { VState(_, ρ, σᵥ, σₖ, k, t1) }
      case EState(Lam(x, e), ρ, σᵥ, σₖ, k, t) =>
        VState(Clo(Lam(x, e), ρ), ρ, σᵥ, σₖ, k, t1)
      // push continuation to KStore
      case EState(e@UnaryOp(op, e1), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(e, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        EState(e, ρ, σᵥ, σₖ1, KUnaryOp(op, ρ, α), t1)
      case EState(e@BinOp(op, e1, e2), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(e, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        EState(e1, ρ, σᵥ, σₖ1, KBinOpR(op, e2, ρ, α), t1)
      case EState(e@App(f, arg), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(e, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        EState(f, ρ, σᵥ, σₖ1, KArg(arg, ρ, α), t1)
      case EState(e@Let(x, rhs, body), ρ, σᵥ, σₖ, k, t) =>
        val α = allocKont(e, t1)
        val σₖ1 = σₖ ⊔ Map(α → Set(k))
        EState(rhs, ρ, σᵥ, σₖ1, KLet(x, ρ, body, α), t1)
      case EState(e@Letrec(x, rhs, body), ρ, σᵥ, σₖ, k, t) =>
        val αb = allocBind(x, t1)
        val αk = allocKont(e, t1)
        val ρ1 = ρ + (x → αb)
        val σᵥ1 = σᵥ ⊔ Map(αb → Set())
        val σₖ1 = σₖ ⊔ Map(αk → Set(k))
        EState(rhs, ρ1, σᵥ1, σₖ1, KLetrec(x, ρ, body, αk), t1)

  def drive(todo: List[State], seen: Set[State]): Set[State] =
    if (todo.isEmpty) seen
    else
      val s::rest = todo
      if seen(s) then drive(rest, seen)
      else if isDone(s) then drive(rest, seen + s)
      else {
        val succ = step(s)
        order += (s -> succ)
        drive(succ.toList ++ rest, seen + s)
      }

  def inject(e: Expr): State = EState(e, Map(), Map(), Map(), KHalt(), List())

  def run(e: Expr): Set[State] =
    val initial = inject(e)
    drive(List(initial), Set())

class Analyzer0CFA extends Analyzer:
  def tick(t: State): Time = List()
  def allocBind(x: String, t: Time): BAddr = BAddr(x, List())
  def allocKont(e: Expr, t: Time): KAddr = KAddr(e, List())
