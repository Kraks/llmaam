package llmaam.aam

import llmaam.benchmarks
import llmaam.syntax.*
import munit.FunSuite

import dev.langchain4j.model.googleai.*
import dev.langchain4j.model.openai.*

def testAnalyzer[T <: Analyzer](analyzer: T, program: Expr, name: String): Unit =
  val states = analyzer.run(program)
  println(s"Total states for ${name}: ${states.size}")
  val transitionStates = analyzer.transitions.foldLeft(Set[State]()) {
    case (acc, (from, lab, to)) => acc + from ++ to
  }
  // Check the recorded "transitions" match the states found
  assert(states == transitionStates)
  analyzer.dumpGraph(s"${name}.dot")

class TestAAM extends FunSuite {
  test("simple app"):
    testAnalyzer(new Analyzer0CFA, benchmarks.app1, "app1")

  test("omega"):
    testAnalyzer(new Analyzer0CFA, benchmarks.omega, "omega")

  test("omega - aac"):
    testAnalyzer(new Analyzer0CFA with AACContAlloc, benchmarks.omega, "omega_aac")

  test("omega2"):
    testAnalyzer(new Analyzer0CFA, benchmarks.omega2, "omega2")

  /* comparing allocation strategies */

  // stack

  test("stack"):
    testAnalyzer(new Analyzer0CFA, benchmarks.stack, "stack")

  test("stack - tgt cont"):
    testAnalyzer(new Analyzer0CFA with TgtContAlloc, benchmarks.stack, "stack_tgtcont")

  test("stack - p4f"):
    testAnalyzer(new Analyzer0CFA with P4FContAlloc, benchmarks.stack, "stack_p4f")

  test("stack - aac"):
    testAnalyzer(new Analyzer0CFA with AACContAlloc, benchmarks.stack, "stack_aac")

  // stack2

  test("stack2"):
    testAnalyzer(new Analyzer0CFA, benchmarks.stack2, "stack2")

  test("stack2 - tgt cont"):
    testAnalyzer(new Analyzer0CFA with TgtContAlloc, benchmarks.stack2, "stack2_tgtcont")

  test("stack2 - p4f"):
    testAnalyzer(new Analyzer0CFA with P4FContAlloc, benchmarks.stack2, "stack2_p4f")

  test("stack2 - aac"):
    testAnalyzer(new Analyzer0CFA with AACContAlloc, benchmarks.stack2, "stack2_aac")

  // binopid

  test("binopid"):
    testAnalyzer(new Analyzer0CFA, benchmarks.binopid, "binopid")

  test("binopid - tgt cont"):
    testAnalyzer(new Analyzer0CFA with TgtContAlloc, benchmarks.binopid, "binopid_tgtcont")

  test("binopid - p4f"):
    testAnalyzer(new Analyzer0CFA with P4FContAlloc, benchmarks.binopid, "binopid_p4f")

  test("binopid - aac"):
    testAnalyzer(new Analyzer0CFA with AACContAlloc, benchmarks.binopid, "binopid_aac")
}

class TestLLM extends FunSuite {
  test("openai-demo"):
    val model = OpenAiChatModel.builder()
      .baseUrl("http://langchain4j.dev/demo/openai/v1")
      .apiKey("demo")
      .modelName("gpt-4o-mini")
      .build()
    val answer = model.chat("Say 'Hello World for Scala 3!'");
    println(answer)
}