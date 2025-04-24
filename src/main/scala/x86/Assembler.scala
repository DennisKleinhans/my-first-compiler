package x86

import java.nio.file.{ Path, Paths, Files }
import sys.process._

def assemble(prog: Program, basename: String): Path = {
  val outPath = Paths.get("out")
  val asmPath = Paths.get("out", basename + ".s")
  val rtsPath = Paths.get("include", "runtime.c")
  val exePath = Paths.get("out", basename + ".exe")

  Files.createDirectories(outPath)
  if (System.getProperty("os.name").contains("Mac")) {
    Files.write(asmPath, formatMacOS(prog).getBytes())
    f"clang -arch x86_64 -o \"${exePath.toString}\" \"${asmPath.toString}\" \"${rtsPath.toString}\"".!!
  } else {
    Files.write(asmPath, format(prog).getBytes())
    f"clang -o \"${exePath.toString}\" \"${asmPath.toString}\" \"${rtsPath.toString}\"".!!
  }
  exePath
}
