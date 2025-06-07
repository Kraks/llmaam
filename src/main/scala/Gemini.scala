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

trait Gemini:
  val geminiAPI = Source.fromFile("GEMINI_AI_KEY").getLines().next()
  val gemini = GoogleAiGeminiChatModel.builder()
    .apiKey(geminiAPI)
    .modelName("gemini-2.5-flash-preview-05-20")
    //.modelName("gemini-2.0-flash")
    .responseFormat(ResponseFormat.JSON)
    .logRequestsAndResponses(true)
    .build()

  def llm: ChatModel = gemini

  def prepareBindAddrQuery(s: State, x: String, t: Time): String =
    s"""
    |{
    |  "state": ${s},
    |  "query-type": "BAddr",
    |  "variable": ${x},
    |  "time": ${t},
    |}
    |""".stripMargin

  def prepareKontAddrQuery(s: State, src: Expr, tgt: Expr, tgtEnv: Env, tgtSt: BStore, t: Time): String =
    s"""
    |{
    |  "state": ${s},
    |  "query-type": "KAddr",
    |  "time": ${t},
    |  "source-expression": ${src},
    |  "target-expression": ${tgt},
    |  "target-environment": ${tgtEnv},
    |  "target-binding-store": ${tgtSt},
    |}
    |""".stripMargin

  def prepareTickQuery(s: State, t: Time): String =
    s"""
    |{
    |  "state": ${s},
    |  "query-type": "Tick",
    |  "time": ${t}
    |}
    |""".stripMargin

  def systemMsg: String = s"""
  |You are an expert in static analysis and abstract interpretation, specifically in the "abstracting abstract machine" approach.
  |Your task is to analyze the process of a small-step abstract interpreter that transforms an abstract machine state into possible successor states.
  |I will provide you the data structures used in the abstract interpretation first,
  |then give your the current analysis state, ask you to analyze it, and ask you to decide the context-sensitivity or
  |return an abstract address used in the allocation.
  |
  |The "State" field is an abstract state, which can be one of the following:
  |enum State:
  |  case EState(e: Expr, ρ: Env, σᵥ: BStore, σₖ: KStore, k: Kont, t: Time)
  |  case VState(v: Value, ρ: Env, σᵥ: BStore, σₖ: KStore, k: Kont, t: Time)
  |  case ErrState()
  |
  |The "Expr" of "State" field is the current program/expression being evaluated, which is defined by the following algebraic data type:
  |enum Expr:
  |  case Lit(n: Int)
  |  case UnaryOp(op: String, arg: Expr)
  |  case BinOp(op: String, lhs: Expr, rhs: Expr)
  |  case Var(x: String)
  |  case Lam(x: String, body: Expr)
  |  case App(f: Expr, arg: Expr)
  |  case Let(x: String, rhs: Expr, body: Expr)
  |  case Letrec(x: String, rhs: Expr, body: Expr)
  |The input program is already renamed so that variable names are unique.
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
  |The "BStore" field is the store mapping abstract binding addresses to sets of abstract values, each map is represented with ->.
  |type BStore = Map[BAddr, Set[Value]]
  |Elements in the set can be either "ℤ" for abstract integers Num() or a closure represented as "⟨lambda_expression, environment⟩".
  |
  |The "KStore" field is the store mapping abstract binding addresses to sets of continuations, each map is represented with ->.
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
  |Your input is a JSON object with the following schema:
  |{
  |  "state": State,
  |  "query-type": "BAddr" or "KAddr" or "Tick",
  |  "variable": String,                  // for BAddr only
  |  "time": Time,                        // for both BAddr and KAddr and "Tick"
  |  "source-expression": Expr,           // for KAddr only
  |  "target-expression": Expr,           // for KAddr only
  |  "target-environment": Env,           // for KAddr only
  |  "target-binding-store": BStore,      // for KAddr only
  |}
  |
  |You should analyze the current "state" and return an abstract address for better analysis precision.
  |You should look at the entries of the existing binding store and continuation store,
  |since if the binding address already exists, reusing it decreases the precision of the analysis.
  |
  |Your output should be only a JSON object with the following schema:
  |{
  |  "reason": string
  |  "query-type": "BAddr" or "KAddr" or "Tick",,
  |  "variable": String,              // for BAddr only
  |  "time": Bool,                    // for both BAddr and KAddr
  |  "source-expression": Bool,       // for KAddr only
  |  "target-expression": Bool,       // for KAddr only
  |  "target-environment": Bool,      // for KAddr only
  |  "target-binding-store": Bool,    // for KAddr only
  |  "k": Int represented as String,  // for Tick only
  |}
  |
  |Field "reason" should explain why you chose this address for better precision by analyzing the current state.
  |The reason you give should be specific to the current state and should not be generic.
  |Field "query-type" should be either "BAddr", "KAddr", or "Tick" according to the "query-type" field in the input query.
  |
  |Only if "query-type" is "BAddr":
  |Field "time" should be true if the binding address should be instrumented with the time (call history).
  |You may set time to false if you think it is not needed for the analysis.
  |Field "variable" should the same variable name as in the input query for binding addresses.
  |
  |Only if "query-type" is "KAddr":
  |Field "source-expression" should be true if the continuation address should be instrumented with the source expression, otherwise false.
  |Field "target-expression" should be true if the continuation address should be instrumented with the target expression, otherwise false.
  |Field "target-environment" should be true if the continuation address should be instrumented with the target environment, otherwise false.
  |Field "target-binding-store" should be true if the continuation address should be instrumented with the target binding store, otherwise false.
  |
  |Only if "query-type" is "Tick":
  |Field "k" should be the number (represented as a quoted string) of context-sensitivity levels to use in the analysis.
  |Given the returned value of "k", the analysis will be k-call-context-sensitive.
  |(i.e. in code the current call expression is prepended to the history, and then the take first k call sites of the whole history).
  |Typically, field "k" is a small number, like 0 or 1. If you think k >= 2 is needed, please justify in the "reason" field.
  |
  |""".stripMargin