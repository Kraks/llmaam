package llmaam.aam

import llmaam.syntax.*
import Expr.*
import Kont.*
import Value.*
import State.*

/* Some baseline AAM allocation strategies.
 *
 * Binding address allocators:
 * - 0CFA: no instrumentation (no context-sensitivity).
 * - k-CFA: tracks the last k call strings as instrumentation (subsumes 0CFA).
 *
 * Continuation address allocators:
 * - `SrcContAlloc`: using the source/previous expression (that causes the allocation) as the continuation address
 *   (as in Systematic abstraction of abstract machines, JFP 12).
 * - `TgtContAlloc`: using the target expression as the continuation address (similar to the
 *   baseline version described in Pushdown Control-Flow Analysis for Free, POPL16).
 * - `P4FContAlloc`: using the target expression and the target environment as the continuation address
 *   (as in Pushdown Control-Flow Analysis for Free, POPL16).
 *   Note: It doesn't work exactly the same as we use direct-style instead of ANF.
 * - `AACContAlloc`: the AAC continuation allocation strategy, it (should) works for direct-style programs
 *   (AAC variant described in Pushdown Control-Flow Analysis for Free, POPL 16).
 */

trait ZeroCFA:
  self: Analyzer =>
  def tick(s: State): Time = List()
  def allocBind(s: State, x: String, t: Time): BAddr = BAddr(x, t)

trait KCFA(k: Int):
  self: Analyzer =>
  def tick(s: State): Time = s match
    case EState(e: App, _, _, _, _, t) => (e :: t).take(k)
    case EState(_, _, _, _, _, t) => t // doesn't tick if not a call
    case VState(_, _, _, _, _, t) => t // value state doesn't tick
    case ErrState() => ???
  def allocBind(s: State, x: String, t: Time): BAddr = BAddr(x, t)

trait SrcContAlloc:
  self: Analyzer =>
  def allocKont(s: State, e1: Expr, ρ1: Env, σᵥ1: BStore, t: Time): KAddr =
    val EState(e, _, _, _, _, _) = s
    KAddr(List(e, t))

trait TgtContAlloc:
  self: Analyzer =>
  def allocKont(s: State, e1: Expr, ρ1: Env, σᵥ1: BStore, t: Time): KAddr =
    val EState(e, _, _, _, _, _) = s
    KAddr(List(e1, t))

trait P4FContAlloc:
  self: Analyzer =>
  def allocKont(s: State, e1: Expr, ρ1: Env, σᵥ1: BStore, t: Time): KAddr =
    val EState(e, _, _, _, _, _) = s
    KAddr(List(e1, ρ1, t))

trait AACContAlloc:
  self: Analyzer =>
  def allocKont(s: State, e1: Expr, ρ1: Env, σᵥ1: BStore, t: Time): KAddr =
    val EState(e, ρ, σ, _, _, _) = s
    KAddr(List(e1, ρ1, e, ρ, σ, t)) // XXX: is ρ necessary?