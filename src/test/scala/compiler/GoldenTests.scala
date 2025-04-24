package compiler

import java.io.File
import java.nio.file.Paths
import munit.FunSuite
import sys.process._

class GoldenTests extends FunSuite {

  val examples = new File("examples")

  examples.listFiles.foreach {
    case f if f.getName.endsWith(".lang") =>
      test(f.toString) {
        val inputPath = f.toPath
        val basename = inputPath.getFileName.toString.replace(".lang", "")
        val checkPath = Paths.get("examples", basename + ".check")
        val expected = readFile(checkPath)
        val exePath = compile(inputPath)
        val output = exePath.toString.!!
        assertNoDiff(output, expected)
      }
    case _ => ()
  }
}
