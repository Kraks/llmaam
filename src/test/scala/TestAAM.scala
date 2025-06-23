package llmaam.aam

import llmaam.benchmarks
import llmaam.syntax.*
import munit.FunSuite

import dev.langchain4j.model.googleai.*
import dev.langchain4j.model.openai.*

def testAnalyzer[T <: Analyzer](analyzer: T, program: Expr, name: String): Unit =
  println(s"Running analysis for ${program}...")
  val states = analyzer.run(program)
  println(s"Analysis summary [${name}]: #State: ${states.size}, #Edges: ${analyzer.transitions.size}")
  val transitionStates = analyzer.transitions.foldLeft(Set[State]()) {
    case (acc, (from, lab, to)) => acc + from ++ to
  }
  // Check the recorded "transitions" match the states found
  assert(states == transitionStates)
  analyzer.dumpGraph(s"${name}.dot", true)

class Playground extends FunSuite {
  //test("stack2 - 2cfa; p4f"):
  //  testAnalyzer(new Analyzer with KCFA(2) with P4FContAlloc, benchmarks.stack2, "stack2_2cfa_p4f")

  testAnalyzer(new Analyzer with LLMAlloc with OpenAI, benchmarks.stack2, "stack2_openai")
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

  test("begin1"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.begin1, "begin1_0cfa")

  test("beginscope"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.beginscope, "beginscope_0cfa")

  test("beginnil"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.beginnil, "beginnil_0cfa")

  test("if1"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.if1, "if1_0cfa")

  test("iferr"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.iferr, "iferr_0cfa")

  test("while1"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.while1, "while1_0cfa")

  test("while2"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.while2, "while2_0cfa")

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

  test("stack2 - 1cfa; tgt cont"):
    testAnalyzer(new Analyzer with KCFA(1) with TgtContAlloc, benchmarks.stack2, "stack2_1cfa")

  test("stack2 - 0cfa; p4f"):
    testAnalyzer(new Analyzer with ZeroCFA with P4FContAlloc, benchmarks.stack2, "stack2_0cfa_p4f")

  test("stack2 - 1cfa; p4f"):
    testAnalyzer(new Analyzer with KCFA(1) with P4FContAlloc, benchmarks.stack2, "stack2_1cfa_p4f")

  // binopid

  test("binopid"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.binopid, "binopid_0cfa")

  test("binopid - tgt cont"):
    testAnalyzer(new Analyzer with ZeroCFA with TgtContAlloc, benchmarks.binopid, "binopid_0cfa_tgtcont")

  test("binopid - p4f"):
    testAnalyzer(new Analyzer with ZeroCFA with P4FContAlloc, benchmarks.binopid, "binopid_0cfa_p4f")

  test("binopid - aac"):
    testAnalyzer(new Analyzer with ZeroCFA with AACContAlloc, benchmarks.binopid, "binopid_0cfa_aac")

  // shadow

  test("shadowapp"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.shadowapp, "shadowapp_0cfa")

  test("shadowapp - kcfa"):
    testAnalyzer(new Analyzer with KCFA(1) with SrcContAlloc, benchmarks.shadowapp, "shadowapp_2cfa")

  test("shadowbinop"):
    testAnalyzer(new Analyzer with ZeroCFA with SrcContAlloc, benchmarks.shadowbinop, "shadowbinop_0cfa")
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

class TestSyntax extends FunSuite {
  test("syntax"):
    println(benchmarks.begin1)
    println(benchmarks.beginscope)
    println(benchmarks.beginnil)
    println(benchmarks.if1)
    println(benchmarks.while1)
    println(benchmarks.while2)
}