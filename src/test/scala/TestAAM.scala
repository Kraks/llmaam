package llmaam.aam

import llmaam.benchmarks
import llmaam.syntax.*
import llmaam.frontend.scm.SchemeParser
import llmaam.frontend.scm.toCore
import munit.FunSuite

import dev.langchain4j.model.googleai.*
import dev.langchain4j.model.openai.*

def testAnalyzer[T <: Analyzer](analyzer: T, program: Expr, name: String, printNode: Boolean = true): Unit =
  //println(s"Running analysis for ${program}...")
  val states = analyzer.run(program)
  println(s"Analysis summary [${name}]: #State: ${states.size}, #Edges: ${analyzer.transitions.size}")
  val transitionStates = analyzer.transitions.foldLeft(Set[State]()) {
    case (acc, (from, lab, to)) => acc + from ++ to
  }
  // Check the recorded "transitions" match the states found
  assert(states == transitionStates)
  analyzer.dumpGraph(s"${name}.dot", printNode)

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
}

def benchmark(file: String, name: String): Unit = {
  val prog = SchemeParser.parseFile(file).get.toCore

  testAnalyzer(new Analyzer with KCFA(1) with P4FContAlloc, prog, s"${name}_1cfa_p4f", false)
  /*
  testAnalyzer(new Analyzer with KCFA(0) with P4FContAlloc, prog, s"${name}_0cfa_p4f", false)
  testAnalyzer(new Analyzer with KCFA(0) with AACContAlloc, prog, s"${name}_0cfa_aac", false)
  testAnalyzer(new Analyzer with KCFA(0) with SrcContAlloc, prog, s"${name}_0cfa_src", false)
  testAnalyzer(new Analyzer with KCFA(0) with TgtContAlloc, prog, s"${name}_0cfa_tgt", false)

  testAnalyzer(new Analyzer with KCFA(1) with SrcContAlloc, prog, s"${name}_1cfa_src", false)
  testAnalyzer(new Analyzer with KCFA(1) with AACContAlloc, prog, s"${name}_1cfa_aac", false)
  testAnalyzer(new Analyzer with KCFA(1) with P4FContAlloc, prog, s"${name}_1cfa_p4f", false)
  testAnalyzer(new Analyzer with KCFA(1) with TgtContAlloc, prog, s"${name}_1cfa_tgt", false)

  testAnalyzer(new Analyzer with LLMAlloc with OpenAI, prog, s"${name}_gpt4o_try1", false)
  testAnalyzer(new Analyzer with LLMAlloc with OpenAI, prog, s"${name}_gpt4o_try2", false)
  testAnalyzer(new Analyzer with LLMAlloc with OpenAI, prog, s"${name}_gpt4o_try3", false)
  */
}

class kcfa2 extends FunSuite {
  // Analysis summary [kcfa2_0cfa_p4f]: #State: 307, #Edges: 306
  // Analysis summary [kcfa2_0cfa_aac]: #State: 203, #Edges: 202
  // Analysis summary [kcfa2_0cfa_src]: #State: 246, #Edges: 245
  // Analysis summary [kcfa2_0cfa_tgt]: #State: 307, #Edges: 306

  // Analysis summary [kcfa2_1cfa_src]: #State: 12189, #Edges: 12117
  // Analysis summary [kcfa2_1cfa_aac]: #State: 336, #Edges: 324
  // Analysis summary [kcfa2_1cfa_p4f]: TO
  // Analysis summary [kcfa2_1cfa_tgt]: TO

  //Analysis summary [kcfa2_gpt4o]: #State: 133, #Edges: 131
  //Analysis summary [kcfa2_gpt40]: #State: 153, #Edges: 150
  //Analysis summary [kcfa2_gpt4o]: #State: 312, #Edges: 303
  //Analysis summary [kcfa2_gpt4o]: #State: 272, #Edges: 265
  benchmark("benchmarks/kcfa/kcfa-worst-case-2.scm", "kcfa2")
}

class kcfa3 extends FunSuite {
  // Analysis summary [kcfa3_0cfa_p4f]: #State: 651, #Edges: 650
  // Analysis summary [kcfa3_0cfa_aac]: #State: 550, #Edges: 549
  // Analysis summary [kcfa3_0cfa_src]: #State: 517, #Edges: 516

  // XXX: others are too slow... since the benchmark is designed to be
  // worst-case (to the top of the lattice)...
  benchmark("benchmarks/kcfa/kcfa-worst-case-3.scm", "kcfa3")
}

class sat extends FunSuite {
  benchmark("benchmarks/gcfa2/sat.scm", "sat")
}

class loop2 extends FunSuite {
  /*
  Analysis summary [loop2_0cfa_p4f]: #State: 110, #Edges: 108
  Analysis summary [loop2_0cfa_aac]: #State: 110, #Edges: 108
  Analysis summary [loop2_0cfa_src]: #State: 149, #Edges: 146
  Analysis summary [loop2_0cfa_tgt]: #State: 219, #Edges: 215
  Analysis summary [loop2_1cfa_src]: #State: 108, #Edges: 106
  Analysis summary [loop2_1cfa_aac]: #State: 108, #Edges: 106
  Analysis summary [loop2_1cfa_p4f]: #State: 108, #Edges: 106
  Analysis summary [loop2_1cfa_tgt]: #State: 108, #Edges: 106
  Analysis summary [loop2_gpt4o_try1]: #State: 108, #Edges: 106
  Analysis summary [loop2_gpt4o_try2]: #State: 108, #Edges: 106
  Analysis summary [loop2_gpt4o_try3]: #State: 108, #Edges: 106
  */
  benchmark("benchmarks/gcfa2/loop2.scm", "loop2")
}

class idid extends FunSuite {
  //Analysis summary [stack2_0cfa_p4f]: #State: 42, #Edges: 41
  testAnalyzer(new Analyzer with KCFA(0) with P4FContAlloc, benchmarks.stack2, "stack2_0cfa_p4f")
  //Analysis summary [stack2_0cfa_aac]: #State: 33, #Edges: 32
  testAnalyzer(new Analyzer with KCFA(0) with AACContAlloc, benchmarks.stack2, "stack2_0cfa_aac")
  //Analysis summary [stack2_0cfa_srccont]: #State: 42, #Edges: 41
  testAnalyzer(new Analyzer with KCFA(0) with SrcContAlloc, benchmarks.stack2, "stack2_0cfa_srccont")

  //Analysis summary [stack2_1cfa_p4f]: #State: 33, #Edges: 32
  testAnalyzer(new Analyzer with KCFA(1) with P4FContAlloc, benchmarks.stack2, "stack2_1cfa_p4f")
  //Analysis summary [stack2_1cfa_aac]: #State: 33, #Edges: 32
  testAnalyzer(new Analyzer with KCFA(1) with AACContAlloc, benchmarks.stack2, "stack2_1cfa_aac")
  //Analysis summary [stack2_1cfa_srccont]: #State: 42, #Edges: 41
  testAnalyzer(new Analyzer with KCFA(1) with SrcContAlloc, benchmarks.stack2, "stack2_1cfa_srccont")

  //Analysis summary [kcfa2_gpt4o_try1]: #State: 33, #Edges: 32
  //Analysis summary [kcfa2_gpt4o_try2]: #State: 33, #Edges: 32
  //Analysis summary [kcfa2_gpt4o_try3]: #State: 33, #Edges: 32
  //Analysis summary [kcfa2_gpt4o_try4]: #State: 33, #Edges: 32
  testAnalyzer(new Analyzer with LLMAlloc with OpenAI, benchmarks.stack2, "kcfa2_gpt4o_try1", false)
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