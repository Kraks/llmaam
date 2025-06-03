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
  analyzer.dumpGraph(s"${name}.dot", false)

class Playground extends FunSuite {
  //test("stack2 - 2cfa; p4f"):
  //  testAnalyzer(new Analyzer with KCFA(2) with P4FContAlloc, benchmarks.stack2, "stack2_2cfa_p4f")

  //testAnalyzer(new Analyzer with LLMAllocator, benchmarks.omega, "llm_exp")
}

class TestAAM extends FunSuite {
  test("simple app"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.app1, "app1_0cfa")

  test("omega"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.omega, "omega_0cfa")

  test("omega - aac"):
    testAnalyzer(new Analyzer with ZeroCFA with AACContAlloc, benchmarks.omega, "omega_0cfa_aac")

  test("omega2"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.omega2, "omega2_0cfa")

  /* comparing allocation strategies */

  // stack

  test("stack"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.stack, "stack_0cfa")

  test("stack - tgt cont"):
    testAnalyzer(new Analyzer with ZeroCFA with TgtContAlloc, benchmarks.stack, "stack_0cfa_tgtcont")

  test("stack - p4f"):
    testAnalyzer(new Analyzer with ZeroCFA with P4FContAlloc, benchmarks.stack, "stack_0cfa_p4f")

  test("stack - aac"):
    testAnalyzer(new Analyzer with ZeroCFA with AACContAlloc, benchmarks.stack, "stack_0cfa_aac")

  // stack2

  test("stack2 - src cont"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.stack2, "stack2_0cfa_srccont")

  test("stack2 - aac"):
    testAnalyzer(new Analyzer with ZeroCFA with AACContAlloc, benchmarks.stack2, "stack2_0cfa_aac")

  // Observe that only 2-CFA + P4F can recover the full precision of the analysis of stack2:

  test("stack2 - 0cfa; tgt cont"):
    testAnalyzer(new Analyzer with ZeroCFA with TgtContAlloc, benchmarks.stack2, "stack2_0cfa")

  test("stack2 - 2cfa; tgt cont"):
    testAnalyzer(new Analyzer with KCFA(2) with TgtContAlloc, benchmarks.stack2, "stack2_2cfa")

  test("stack2 - 0cfa; p4f"):
    testAnalyzer(new Analyzer with ZeroCFA with P4FContAlloc, benchmarks.stack2, "stack2_0cfa_p4f")

  test("stack2 - 2cfa; p4f"):
    testAnalyzer(new Analyzer with KCFA(2) with P4FContAlloc, benchmarks.stack2, "stack2_2cfa_p4f")

  // binopid

  test("binopid"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.binopid, "binopid_0cfa")

  test("binopid - tgt cont"):
    testAnalyzer(new Analyzer with ZeroCFA with TgtContAlloc, benchmarks.binopid, "binopid_0cfa_tgtcont")

  test("binopid - p4f"):
    testAnalyzer(new Analyzer with ZeroCFA with P4FContAlloc, benchmarks.binopid, "binopid_0cfa_p4f")

  test("binopid - aac"):
    testAnalyzer(new Analyzer with ZeroCFA with AACContAlloc, benchmarks.binopid, "binopid_0cfa_aac")
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