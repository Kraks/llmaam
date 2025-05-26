package llmaam.aam

import llmaam.benchmarks
import munit.FunSuite

class MySuite extends FunSuite {

  test("simple app"):
    val analyzer = new Analyzer0CFA
    val states = analyzer.run(benchmarks.app1)
    println(states.size)
    println(analyzer.order)
    analyzer.dumpGraph("app1.dot")
}
