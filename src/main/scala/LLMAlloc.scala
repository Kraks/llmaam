package llmaam.aam

import llmaam.syntax.Expr

import scala.io.Source
import scala.collection.mutable.{ListBuffer, HashMap}
import upickle.default.*

import dev.langchain4j.model.chat.ChatModel
import dev.langchain4j.model.googleai.*
import dev.langchain4j.model.openai.*
import dev.langchain4j.data.message.SystemMessage
import dev.langchain4j.data.message.UserMessage
import dev.langchain4j.model.chat.request.ResponseFormat

import State.*

trait LLMAlloc:
  self: Analyzer =>

  // Need to be provided:
  def llm: ChatModel
  def prepareBindAddrQuery(s: State, x: String, t: Time): String
  def prepareKontAddrQuery(s: State, src: Expr, tgt: Expr, tgtEnv: Env, tgtSt: BStore, t: Time): String
  def prepareTickQuery(s: State, t: Time): String
  def systemMsg: String

  def queryLLM(q: String): Map[String, String] =
    println(s"Query: $q")
    val answer = llm.chat(SystemMessage.from(systemMsg), UserMessage.from(q))
    println(s"Answer: ${answer.aiMessage.text}")
    val parsed = read[Map[String, String]](answer.aiMessage.text)
    //println(s"Parsed: ${parsed}")
    parsed

  def tick(s: State): Time =
    s match
      case EState(e: Expr.App, _, _, _, _, t) =>
        val parsed = queryLLM(prepareTickQuery(s, t))
        assert(parsed("query-type") == "Tick", s"Expected Tick, got ${parsed("query-type")}")
        val k = parsed("k").toInt
        (e :: t).take(k)
      case EState(_, _, _, _, _, t) => t // doesn't tick if not a call
      case VState(_, _, _, _, _, t) => t // value state doesn't tick
      case ErrState() => ???

  def allocBind(s: State, x: String, t: Time): BAddr =
    val parsed = queryLLM(prepareBindAddrQuery(s, x, t))
    assert(parsed("query-type") == "BAddr", s"Expected BAddr, got ${parsed("query-type")}")
    if (parsed("time") == "true") BAddr(x, t)
    else BAddr(x, List())

  def allocKont(s: State, tgt: Expr, tgtEnv: Env, tgtSt: BStore, t: Time): KAddr =
    val EState(e, ρ, σ, _, _, _) = s
    val parsed = queryLLM(prepareKontAddrQuery(s, e, tgt, tgtEnv, tgtSt, t))
    assert(parsed("query-type") == "KAddr", s"Expected KAddr, got ${parsed("type")}")
    val inst: ListBuffer[Option[Any]] = new ListBuffer()
    inst.append(if (parsed("source-expression") == "true") Some(e) else None)
    inst.append(if (parsed("target-expression") == "true") Some(tgt) else None)
    inst.append(if (parsed("target-environment") == "true") Some(tgtEnv) else None)
    inst.append(if (parsed("target-binding-store") == "true") Some(tgtSt) else None)
    inst.append(if (parsed("time") == "true") Some(t) else None)
    KAddr(inst.toList)
