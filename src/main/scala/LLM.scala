package llmaam.aam

import llmaam.syntax.Expr

import scala.io.Source
import upickle.default.*

import dev.langchain4j.model.googleai.*
import dev.langchain4j.model.openai.*
import dev.langchain4j.data.message.SystemMessage
import dev.langchain4j.data.message.UserMessage
import dev.langchain4j.model.chat.request.ResponseFormat

trait LLMAllocator:
  self: Analyzer =>
  val geminiAPI = Source.fromFile("GEMINI_AI_KEY").getLines().next()
  val gemini = GoogleAiGeminiChatModel.builder()
    .apiKey(geminiAPI)
    .modelName("gemini-2.5-flash-preview-05-20")
    //.modelName("gemini-2.0-flash")
    .responseFormat(ResponseFormat.JSON)
    .logRequestsAndResponses(true)
    .build()
  def prepareBindAddrQuery(s: State, x: String, t: Time): String =
    s"""
    |{
    |  "state": ${s},
    |  "query": "allocBind(state, $x, $t)"
    |}
    |""".stripMargin

  def tick(t: State): Time = List()
  def allocBind(currentState: State, x: String, t: Time): BAddr =
    val query = prepareBindAddrQuery(currentState, x, t)
    println(s"Query: $query")
    val answer = gemini.chat(SystemMessage.from(initPrompt), UserMessage.from(query))
    println(s"Answer: ${answer.aiMessage.text}")
    // TODO: need to reify the answer to Scala object
    BAddr(x, t)
  def allocKont(currentState: State, tgtExpr: Expr, tgtEnv: Env, tgtBStore: BStore, newTime: Time): KAddr = KAddr(tgtExpr, List())

def initPrompt: String =
  s"""
  |Your task is to analyze the process of static analysis or abstract interpretation simulated by an abstract machine.
  |I will provide you the abstract state, and ask you to analyze it, and return an abstract address used in the allocation.
  |
  |Your input is a JSON object with the following schema:
  |{
  |  "state": State,
  |  "query": string
  |}
  |
  |The "State" field is an abstract state, which can be one of the following:
  |enum State:
  |  case EState(e: Expr, ρ: Env, σᵥ: BStore, σₖ: KStore, k: Kont, t: Time)
  |  case VState(v: Value, ρ: Env, σᵥ: BStore, σₖ: KStore, k: Kont, t: Time)
  |  case ErrState()
  |
  |The "Expr" of "State" field is the current expression being evaluated, which is defined by the following algebraic data type:
  |  enum Expr:
  |  case Lit(n: Int)
  |  case UnaryOp(op: String, arg: Expr)
  |  case BinOp(op: String, lhs: Expr, rhs: Expr)
  |  case Var(x: String)
  |  case Lam(x: String, body: Expr)
  |  case App(f: Expr, arg: Expr)
  |  case Let(x: String, rhs: Expr, body: Expr)
  |  case Letrec(x: String, rhs: Expr, body: Expr)
  |
  |The "Value" of "State" field is the value being evaluated, which is defined by the following algebraic data type:
  |enum Value:
  |  case Num()
  |  case Clo(lam: Expr.Lam, ρ: Env)
  |
  |The "Env" field is the environment mapping variable names to their abstract addresses BAddr.
  |type Env = Map[String, BAddr]
  |case class BAddr(x: String, instrumentation: List[Any])
  |
  |The "BStore" field is the store mapping abstract binding addresses to sets of abstract values.
  |type BStore = Map[BAddr, Set[Value]]
  |Elements in the set can be either "ℤ" for abstract integers Num() or a closure represented as "⟨lambda_expression, environment⟩".
  |
  |The "KStore" field is the store mapping abstract binding addresses to sets of continuations.
  |type KStore = Map[KAddr, Set[Kont]]
  |case class KAddr(e: Expr, instrumentation: List[Any])
  |
  |The "Kont"/"continuation" field is the current continuation, which is defined by the following algebraic data type:
  |  enum Kont:
  |  case KHalt()
  |  case KUnaryOp(op: String, ρ: Env, k: KAddr)
  |  case KBinOpR(op: String, rhs: Expr, ρ: Env, k: KAddr)
  |  case KBinOpL(op: String, lhs: Value, k: KAddr)
  |  case KArg(e: Expr, ρ: Env, k: KAddr)
  |  case KFun(lam: Expr.Lam, ρ: Env, k: KAddr)
  |  case KLet(x: String, ρ: Env, body: Expr, k: KAddr)
  |  case KLetrec(x: String, xa: BAddr, ρ: Env, body: Expr, k: KAddr)
  |
  |The "Time" field is a list of program call sites expression that represent the history of
  |calls leading to the current state (context sensitivity).
  |type Time = List[Expr]
  |
  |The "query" field is one of the following, asking you to synthesize an abstract address using the constructor of BAddr or KAddr:
  |  def allocBind(x: String, t: Time) = {BAddr expression}
  |  def allocKont(currentState: State, tgtExpr: Expr, tgtEnv: Env, tgtBStore: BStore, newTime: Time) = {KAddr expression}
  |
  |Your output should be only a JSON object with the following schema:
  |{
  |  "reason": string
  |  "result": BAddr or KAddr expression using constructors
  |}
  |The "reason" field should explain why you chose this address, and the "result" field should be the abstract address you synthesized
  |using the constructor of BAddr or KAddr given above.
  |""".stripMargin

