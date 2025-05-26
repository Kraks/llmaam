package llmaam

import dev.langchain4j.model.googleai.*
import dev.langchain4j.model.openai.*

import scala.io.Source

@main def hello(): Unit =
  val geminiAPI = Source.fromFile("GEMINI_AI_KEY").getLines().next()

  val gemini = GoogleAiGeminiChatModel.builder()
    .apiKey(geminiAPI)
    .modelName("gemini-2.0-flash")
    .build()

  val answer = gemini.chat("Say 'Hello World for Scala 3!'")
  println(answer)
