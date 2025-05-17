package compiler

import lang.SExp
import lang.SExp.*
import lang.parse
import x86.Program
import x86.format
import x86.assemble

import java.nio.file.{Path, Paths}
import scala.io.StdIn

@main
def main(): Unit =
  val path = "examples/print_42.lang"
  compile(Paths.get(path))

def compile(input: Path): Path = {
  val basename = input.getFileName.toString.replace(".lang", "")
  val source = readFile(input)
  val target = replaceMeWithTheActualCompilation(parse(source))
  assemble(target, basename)
}

def replaceMeWithTheActualCompilation(prog: SExp): Program = {
  val progOut = LVarReader.fromSExpToModule(prog)
  val progOut2 = simplifyModule(progOut, NameGenerator())
  val progOut3 = selectInstructions(progOut2)
  val progOut4 = assignHomes(progOut3)
  val prelude = List(
    x86.Instr.PushQ(x86.Arg.Register(x86.Reg.Rbp)),
    x86.Instr
      .MovQ(x86.Arg.Register(x86.Reg.Rsp), x86.Arg.Register(x86.Reg.Rbp)),
    x86.Instr.SubQ(x86.Arg.Immediate(128), x86.Arg.Register(x86.Reg.Rsp))
  )
  val conclusion = List(
    x86.Instr.AddQ(x86.Arg.Immediate(128), x86.Arg.Register(x86.Reg.Rsp)),
    x86.Instr.PopQ(x86.Arg.Register(x86.Reg.Rbp)),
    x86.Instr.RetQ
  )
  val progOut5 = prelude ++ progOut4 ++ conclusion
  x86.Program(Map("main" -> progOut5))
}

def readFile(path: Path): String = {
  val source = scala.io.Source.fromFile(path.toFile)
  try source.getLines.mkString("\n")
  finally source.close()
}
