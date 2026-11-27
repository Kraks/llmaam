package llmaam.aam

import scala.collection.mutable.{ListBuffer, HashMap}
import scala.annotation.tailrec
import llmaam.frontend.anf.{Expr, AExpr, CExpr, Let}

import CExpr.*

// ANF operands are atomic. Only a pending let result needs a continuation frame.
enum Kont:
  case KHalt()
  case KLet(x: String, ρ: Env, body: Expr, k: KAddr)

import Kont.*

def kontName(k: Kont): String = k match
  case KHalt() => "Halt"
  case KLet(x, _, _, _) => s"KLet($x)"

type Lambda = AExpr.Lam | AExpr.RestLam

// A finite, type-based abstract domain.
enum AbsVal:
  case Num()
  case Bool()
  case CharVal()
  case SymVal()
  case ListVal() // FIXME
  case UnitVal()
  case Prim(op: String)
  case Clo(lam: Lambda, ρ: Env)

  override def toString(): String = this match
    case Num() => "ℤ"
    case Bool() => "𝔹"
    case CharVal() => "Char"
    case SymVal() => "Symbol"
    case ListVal() => "List" // FIXME: this is too coarse
    case UnitVal() => "()"
    case Prim(op) => s"Primitive($op)"
    case Clo(lam, ρ) => s"⟨$lam, $ρ⟩"

import AbsVal.*

private val arithmetic = Set("+", "-", "*", "/", "%")
private val relations = Set(">", "<", ">=", "<=", "==", "!=", "=")
private val logic = Set("&", "|", "and", "or", "!", "not")
private val primitives = arithmetic ++ relations ++ logic ++
  Set("display", "input", "list", "quote", "eq?", "eqv?", "equal?")

def evalPrimOp(op: String, vs: List[Set[AbsVal]]): Set[AbsVal] = op match
  case "display" if vs.size == 1 => Set(UnitVal())
  case "input" if vs.isEmpty => Set(Num())
  case "list" => Set(ListVal()) // FIXME
  case "quote" if vs.size == 1 => vs.head // FIXME
  case "eq?" | "eqv?" | "equal?" if vs.size == 2 => Set(Bool())
  case "not" | "!" if vs.size == 1 => Set(Bool())
  case "and" | "or" | "&" | "|" if vs.forall(_ == Set(Bool())) => Set(Bool())
  case "+" | "*" if vs.forall(_ == Set(Num())) => Set(Num())
  case "-" | "/" if vs.nonEmpty && vs.forall(_ == Set(Num())) => Set(Num())
  case "%" if vs.size == 2 && vs.forall(_ == Set(Num())) => Set(Num())
  case o if relations(o) && vs.size >= 2 && vs.forall(_ == Set(Num())) => Set(Bool())
  case _ =>
    println(s"Warning: unknown primitive operation ($op ${vs.map(_.toString).mkString(", ")})")
    Set()

case class BAddr(x: String, instrumentation: List[Any] = List())
case class KAddr(instrumentation: List[Any] = List())

type Env = Map[String, BAddr]
type BStore = Map[BAddr, Set[AbsVal]]
type KStore = Map[KAddr, Set[Kont]]
type Time = List[Expr]

enum State:
  case EState(e: Expr, ρ: Env, σᵥ: BStore, σₖ: KStore, k: Kont, t: Time)
  case VState(v: AbsVal, ρ: Env, σᵥ: BStore, σₖ: KStore, k: Kont, t: Time)
  case ErrState()

import State.*

abstract class Analyzer:
  type Label = String

  def tick(s: State): Time
  def allocBind(s: State, x: String, t: Time): BAddr
  // The target is the callee body or selected branch, with its environment/store.
  def allocKont(s: State, e1: Expr, ρ1: Env, σᵥ1: BStore, t: Time): KAddr

  val transitions: ListBuffer[(State, Label, Set[State])] = ListBuffer()

  def isAtomic(e: Expr): Boolean = e.isInstanceOf[AExpr]

  def isDone(s: State): Boolean = s match
    case VState(_, _, _, _, KHalt(), _) | ErrState() => true
    case _ => false

  def evalAtom(a: AExpr, ρ: Env, σᵥ: BStore): Set[AbsVal] = a match
    case AExpr.IntLit(_) | AExpr.FloatLit(_) => Set(Num())
    case AExpr.BoolLit(_) => Set(Bool())
    case AExpr.CharLit(_) => Set(CharVal())
    case AExpr.Sym(_) => Set(SymVal())
    case AExpr.Void() => Set(UnitVal())
    case lam: AExpr.Lam => Set(Clo(lam, ρ))
    case lam: AExpr.RestLam => Set(Clo(lam, ρ))
    case AExpr.Var(x) => ρ.get(x) match
      case Some(addr) => σᵥ(addr)
      case None if primitives(x) => Set(Prim(x))
      case None =>
        println(s"Warning: unbound variable $x")
        Set()

  def step(s: State): (Label, Set[State]) = s match
    case VState(v, _, σᵥ, σₖ, KLet(x, ρ, body, kaddr), t) =>
      val addr = allocBind(s, x, t)
      val ρ1 = ρ + (x -> addr)
      val σᵥ1 = σᵥ ⊔ Map(addr -> Set(v))
      ("let-body", σₖ(kaddr).map { k =>
        EState(body, ρ1, σᵥ1, σₖ, k, t)
      })
    case VState(_, _, _, _, KHalt(), _) => ("halt", Set.empty)
    case ErrState() => ("error", Set.empty)
    case EState(a: AExpr, ρ, σᵥ, σₖ, k, _) =>
      val t1 = tick(s)
      val vs = evalAtom(a, ρ, σᵥ)
      if vs.isEmpty then ("var-unbound", Set(ErrState()))
      else ("atom-eval", vs.map(v => VState(v, ρ, σᵥ, σₖ, k, t1)))
    case EState(Let(x, Value(a), body), ρ, σᵥ, σₖ, k, _) =>
      val t1 = tick(s)
      val vs = evalAtom(a, ρ, σᵥ)
      if vs.isEmpty then ("var-unbound", Set(ErrState()))
      else ("let-value", vs.map { v =>
            val addr = allocBind(s, x, t1)
            val ρ1 = ρ + (x -> addr)
            val σᵥ1 = σᵥ ⊔ Map(addr -> Set(v))
            EState(body, ρ1, σᵥ1, σₖ, k, t1)
          })
    case EState(Let(x, App(fun, args), body), ρ, σᵥ, σₖ, k, _) =>
      val t1 = tick(s)
      val funVals = evalAtom(fun, ρ, σᵥ) // FIXME:
      val argVals = args.map(evalAtom(_, ρ, σᵥ)) // FIXME: error handling
      if (funVals.isEmpty || argVals.exists(_.isEmpty)) then ("var-unbound", Set(ErrState()))
      else
        val succs = funVals.flatMap:
          case Prim(op) =>
            val vs = evalPrimOp(op, argVals)
            vs.map(VState(_, ρ, σᵥ, σₖ, k, t1))
          case Clo(lam, ρf) =>
            val (params, rest, target) = lam match
              case AExpr.Lam(ps, e) => (ps, None, e)
              case AExpr.RestLam(ps, r, e) => (ps, Some(r), e)
            val bindings = params.zip(argVals) ++ rest.toList.map(_ -> Set(ListVal()))
            val (ρ1, σᵥ1) = bindings.foldLeft((ρf, σᵥ)):
              case ((ρ1, σᵥ1), (x, vs)) =>
                val addr = allocBind(s, x, t1)
                val ρ2 = ρ1 + (x -> addr)
                val σᵥ2 = σᵥ1 ⊔ Map(addr -> vs)
                (ρ2, σᵥ2)
            val kaddr = allocKont(s, target, ρ1, σᵥ, t1)
            val σₖ1 = σₖ ⊔ Map(kaddr -> Set(k))
            Set(EState(target, ρ1, σᵥ1, σₖ1, KLet(x, ρ, body, kaddr), t1))
          //case _ => Set(ErrState())
        ("app", if succs.isEmpty then Set(ErrState()) else succs)
    case EState(Let(x, If(cnd, thn, els), body), ρ, σᵥ, σₖ, k, _) =>
      val t1 = tick(s)
      val cndVal = evalAtom(cnd, ρ, σᵥ)
      if cndVal.isEmpty then ("var-unbound", Set(ErrState()))
      else
        val branches = cndVal.flatMap:
          case Bool() => Set(thn, els)
          case _ => Set(thn)
        ("if-branch", branches.map: br =>
          val kaddr = allocKont(s, br, ρ, σᵥ, t1)
          val σₖ1 = σₖ ⊔ Map(kaddr -> Set(k))
          EState(br, ρ, σᵥ, σₖ1, KLet(x, ρ, body, kaddr), t1)
        )
    case EState(Let(x, SetVar(y, ae), body), ρ, σᵥ, σₖ, k, _) =>
      val vs = evalAtom(ae, ρ, σᵥ)
      if vs.isEmpty then ("var-unbound", Set(ErrState()))
      else
        val α = ρ(y)
        ("set", Set(EState(body, ρ, σᵥ ⊔ Map(α -> vs), σₖ, k, tick(s))))

  @tailrec final def drive(todo: List[State], seen: Set[State]): Set[State] = todo match
    case Nil => seen
    case s :: rest =>
      println(s"Todo: ${todo.size}, Seen: ${seen.size}")
      if seen(s) then drive(rest, seen)
      else if isDone(s) then drive(rest, seen + s)
      else
        val (label, successors) = step(s)
        transitions.append((s, label, successors))
        drive(successors.toList ++ rest, seen + s)

  def inject(e: Expr): State = EState(e, Map.empty, Map.empty, Map.empty, KHalt(), Nil)

  def run(e: Expr): Set[State] =
    transitions.clear()
    drive(List(inject(e)), Set.empty)

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
            writer.println(s"""  ${n} [label="${n}|VState(${v}, ${kontName(k)})"];""")
          case ErrState() =>
            writer.println(s"""  ${n} [label="${n}|ErrState()"];""")
    }
    writer.println("}")
    writer.close()
    val command = s"dot -Tpdf result/${filename} -o result/${filename.replace(".dot", ".pdf")}"
    println("Executing command: " + command)
    command.!