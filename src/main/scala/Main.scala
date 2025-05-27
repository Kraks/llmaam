package llmaam

import dev.langchain4j.model.googleai.*
import dev.langchain4j.model.openai.*
import dev.langchain4j.model.chat.request.ResponseFormat

import scala.io.Source

@main def hello(): Unit =
  val geminiAPI = Source.fromFile("GEMINI_AI_KEY").getLines().next()

  val gemini = GoogleAiGeminiChatModel.builder()
    .apiKey(geminiAPI)
    .modelName("gemini-2.0-flash")
    //.responseFormat(ResponseFormat.JSON)
    .build()

  val answer = gemini.chat("You are a static analyzer following the AAM (Abstracting Abstract M) semantics. You are given a program in the form of an expression, and you need to analyze it. The program is: letrec f = Lam x (App (Var f) (Lit 1)) in App (Var f) (Lit 2). What is the result of this program?")
  println(answer)
