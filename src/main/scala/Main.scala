package llmaam

import dev.langchain4j.model.googleai.*
import dev.langchain4j.model.openai.*
import dev.langchain4j.data.message.SystemMessage
import dev.langchain4j.data.message.UserMessage
import dev.langchain4j.model.chat.request.ResponseFormat

import scala.io.Source

@main def hello(): Unit =
  val geminiAPI = Source.fromFile("GEMINI_AI_KEY").getLines().next()

  val gemini = GoogleAiGeminiChatModel.builder()
    .apiKey(geminiAPI)
    .modelName("gemini-2.0-flash")
    //.responseFormat(ResponseFormat.JSON)
    .build()

