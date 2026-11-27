package llmaam.aam

import llmaam.frontend.anf.{Expr, Let, CExpr}
import Kont.*
import AbsVal.*
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
 * - `AACContAlloc`: the AAC continuation allocation strategy
 *   (AAC variant described in Pushdown Control-Flow Analysis for Free, POPL 16).
 */

trait ZeroCFA:
  self: Analyzer =>
  def tick(s: State): Time = List()
  def allocBind(s: State, x: String, t: Time): BAddr = BAddr(x, t)

trait KCFA(k: Int):
  self: Analyzer =>
  def tick(s: State): Time = s match
    case EState(e @ Let(_, _: CExpr.App, _), _, _, _, _, t) => (e :: t).take(k)
    case EState(_, _, _, _, _, t) => t // doesn't tick if not a call
    case VState(_, _, _, _, _, t) => t // value state doesn't tick
    case ErrState() => Nil
  def allocBind(s: State, x: String, t: Time): BAddr = BAddr(x, t)

trait SrcContAlloc:
  self: Analyzer =>
  def allocKont(s: State, e1: Expr, ρ1: Env, σᵥ1: BStore, t: Time): KAddr =
    val EState(e, _, _, _, _, _) = s
    KAddr(List(e, t))

trait TgtContAlloc:
  self: Analyzer =>
  def allocKont(s: State, e1: Expr, ρ1: Env, σᵥ1: BStore, t: Time): KAddr =
    KAddr(List(e1, t))

trait SrcTgtContAlloc:
  self: Analyzer =>
  def allocKont(s: State, e1: Expr, ρ1: Env, σᵥ1: BStore, t: Time): KAddr =
    val EState(e, _, _, _, _, _) = s
    KAddr(List(e1, e, t))

trait P4FContAlloc:
  self: Analyzer =>
  def allocKont(s: State, e1: Expr, ρ1: Env, σᵥ1: BStore, t: Time): KAddr =
    KAddr(List(e1, ρ1, t))

trait AACContAlloc:
  self: Analyzer =>
  def allocKont(s: State, e1: Expr, ρ1: Env, σᵥ1: BStore, t: Time): KAddr =
    val EState(e, ρ, σ, _, _, _) = s
    KAddr(List(e1, ρ1, e, ρ, σ)) // XXX: is ρ necessary?