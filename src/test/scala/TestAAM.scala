package llmaam.aam

import llmaam.benchmarks
import munit.FunSuite

import dev.langchain4j.model.googleai.*
import dev.langchain4j.model.openai.*

class MySuite extends FunSuite {

  test("simple app"):
    val analyzer = new Analyzer0CFA
    val states = analyzer.run(benchmarks.app1)
    println(states.size)
    println(analyzer.order)
    analyzer.dumpGraph("app1.dot")

  test("openai-demo"):
    val model = OpenAiChatModel.builder()
      .baseUrl("http://langchain4j.dev/demo/openai/v1")
      .apiKey("demo")
      .modelName("gpt-4o-mini")
      .build()
    val answer = model.chat("Say 'Hello World for Scala 3!'");
    println(answer)
}
