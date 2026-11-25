package llmaam.frontend.scm

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using
import munit.FunSuite
import llmaam.frontend.anf.toANF
import llmaam.frontend.counter.reset

class TestANFBenchmarks extends FunSuite:
  private val excluded = Set(
    // Unsupported syntax forms.
    "benchmarks/oaam/CM.sch",
    "benchmarks/oaam/toplas98/handle.scm",
    "benchmarks/oaam/toplas98/nucleic2.sch",
    "benchmarks/oaam/toplas98/splay.scm"
  )

  private val benchmarks = Using.resource(Files.walk(Path.of("benchmarks"))) { paths =>
    paths.iterator.asScala
      .filter(_.toString.matches(".*\\.(scm|sch)"))
      .filterNot(p => excluded(p.toString))
      .toList.sorted
  }

  benchmarks.foreach { path =>
    test(s"parse and normalize $path"):
      reset
      val ast = SchemeParser.parseFile(path.toString).getOrElse(fail(s"Could not parse $path"))
      // Check that the full transformation completes; this does not execute the program.
      ast.toANF
      ()
  }
