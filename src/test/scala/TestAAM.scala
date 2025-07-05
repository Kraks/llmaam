package llmaam.aam

import llmaam.benchmarks
import llmaam.syntax.*
import llmaam.frontend.scm.SchemeParser
import llmaam.frontend.scm.toCore
import munit.FunSuite

import dev.langchain4j.model.googleai.*
import dev.langchain4j.model.openai.*

def testAnalyzer[T <: Analyzer](analyzer: T, program: Expr, name: String, printNode: Boolean = true): Unit =
  println(s"Running analysis for ${program}...")
  val states = analyzer.run(program)
  println(s"Analysis summary [${name}]: #State: ${states.size}, #Edges: ${analyzer.transitions.size}")
  val transitionStates = analyzer.transitions.foldLeft(Set[State]()) {
    case (acc, (from, lab, to)) => acc + from ++ to
  }
  // Check the recorded "transitions" match the states found
  assert(states == transitionStates)
  //analyzer.dumpGraph(s"${name}.dot", printNode)

class Playground extends FunSuite {
  //test("stack2 - 2cfa; p4f"):
  //  testAnalyzer(new Analyzer with KCFA(2) with P4FContAlloc, benchmarks.stack2, "stack2_2cfa_p4f")

  //testAnalyzer(new Analyzer with LLMAlloc with OpenAI, benchmarks.stack2, "stack2_openai")

  // FIXME: there is a VState not handled, seems because a spurious flow causing a condition to be lambda
  val church = SchemeParser.parseFile("benchmarks/oaam/church.sch").get.toCore
  //testAnalyzer(new Analyzer with KCFA(0) with P4FContAlloc, church, "church_0cfa_p4f", false)

  /*
  Note: these works, but 1CFA is quite slow
  val kcfa5 = SchemeParser.parseFile("benchmarks/kcfa/kcfa-worst-case-5.scm").get.toCore
  testAnalyzer(new Analyzer with KCFA(0) with P4FContAlloc, kcfa5, "kcfa5_0cfa_p4f", false)
  testAnalyzer(new Analyzer with KCFA(0) with SrcContAlloc, kcfa5, "kcfa5_0cfa_src", false)
  testAnalyzer(new Analyzer with KCFA(0) with TgtContAlloc, kcfa5, "kcfa5_0cfa_tgt", false)
  */

  val kcfa2 = SchemeParser.parseFile("benchmarks/kcfa/kcfa-worst-case-2.scm").get.toCore
  testAnalyzer(new Analyzer with KCFA(0) with P4FContAlloc, kcfa2, "kcfa2_0cfa_p4f", false)
  testAnalyzer(new Analyzer with KCFA(0) with AACContAlloc, kcfa2, "kcfa2_0cfa_aac", false)
  testAnalyzer(new Analyzer with KCFA(0) with SrcContAlloc, kcfa2, "kcfa2_0cfa_src", false)
  testAnalyzer(new Analyzer with KCFA(0) with TgtContAlloc, kcfa2, "kcfa2_0cfa_tgt", false)

  testAnalyzer(new Analyzer with KCFA(1) with SrcContAlloc, kcfa2, "kcfa2_1cfa_src", false)
  // XXX: those two are prohibitively slow...
  //testAnalyzer(new Analyzer with KCFA(1) with P4FContAlloc, kcfa2, "kcfa2_1cfa_p4f", false)
  //testAnalyzer(new Analyzer with KCFA(1) with TgtContAlloc, kcfa2, "kcfa2_1cfa_tgt", false)
  testAnalyzer(new Analyzer with KCFA(1) with AACContAlloc, kcfa2, "kcfa2_1cfa_aac", false)
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
    println(benchmarks.if1)
    println(benchmarks.while1)
    println(benchmarks.while2)
}