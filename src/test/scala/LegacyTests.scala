
/*
def benchmark(file: String, name: String): Unit = {
  val prog = SchemeParser.parseFile(file).get.toANF

  testAnalyzer(new Analyzer with KCFA(0) with P4FContAlloc, prog, s"${name}_0cfa_p4f", false)
  testAnalyzer(new Analyzer with KCFA(0) with AACContAlloc, prog, s"${name}_0cfa_aac", false)
  testAnalyzer(new Analyzer with KCFA(0) with SrcContAlloc, prog, s"${name}_0cfa_src", false)
  testAnalyzer(new Analyzer with KCFA(0) with TgtContAlloc, prog, s"${name}_0cfa_tgt", false)

  testAnalyzer(new Analyzer with KCFA(1) with SrcContAlloc, prog, s"${name}_1cfa_src", false)
  testAnalyzer(new Analyzer with KCFA(1) with AACContAlloc, prog, s"${name}_1cfa_aac", false)
  testAnalyzer(new Analyzer with KCFA(1) with P4FContAlloc, prog, s"${name}_1cfa_p4f", false)
  testAnalyzer(new Analyzer with KCFA(1) with TgtContAlloc, prog, s"${name}_1cfa_tgt", false)

  // Analysis summary [mj09_gpt4o_try2]: #State: 199, #Edges: 202
  // Analysis summary [mj09_gpt4o_try2]: #State: 268, #Edges: 259
  // Analysis summary [mj09_gpt4o_try3]: #State: 281, #Edges: 273
  testAnalyzer(new Analyzer with LLMAlloc with OpenAI, prog, s"${name}_gpt4o_try1", false)
  testAnalyzer(new Analyzer with LLMAlloc with OpenAI, prog, s"${name}_gpt4o_try2", false)
  testAnalyzer(new Analyzer with LLMAlloc with OpenAI, prog, s"${name}_gpt4o_try3", false)
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

class mj09 extends FunSuite {
  benchmark("benchmarks/gcfa2/mj09.scm", "mj09")
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
*/