package compiler

import lang.SExp
import lang.SExp.*
import lang.parse
import x86.Program
import x86.format
import x86.assemble

import java.nio.file.{Path, Paths}
import scala.io.StdIn
import analysis.*
import passes.SelectInstructions.selectInstructions
import passes.ExplicateControl.explicateControl
import passes.RemoveComplexOperands.removeComplexOperands
import passes.Shrink.shrink
import passes.PatchInstructions.patchInstructions
import passes.PreludeAndConclusion.generatePreludeAndConclusion
// import passes.RegisterAllocation.{allocateRegisters, basicblockGraph}
import optimization.NoOpElimination.eliminateNoOps

@main
def main(): Unit =
  // val path = "examples/loop_compound.lang"
  // compile(Paths.get(path))

  val program =
    """
  def add(x : int, y : int) -> int {
    return x + y
  }
  b = 2
  print(add(b + 2, 2))
  """

    

  val parsed = parse(program)
  println("parsed: " + parsed)
  val sexped = LCoreReader.fromSExpToModule(parsed)
  println("sexped: " + sexped)
  val shrinked = shrink(sexped)
  val removed = removeComplexOperands(shrinked)
  val controlled = explicateControl(removed)
  val selected = selectInstructions(controlled)
  // val (instrsWithHome, stackSpace) = allocateRegisters(selected)
  println("before removed: " + shrinked)
  println("after removed: " + removed)
  println("controlled: " + controlled)
  println("selected: " + selected)
  // println("after register allocation: " + instrsWithHome)

def compile(input: Path): Path = {
  val basename = input.getFileName.toString.replace(".lang", "")
  val source = readFile(input)
  val sexped = parse(source)
  val target = transformTox86(sexped)
  assemble(target, basename)
}

def transformTox86(prog: SExp): Program = {
  val parsed = LCoreReader.fromSExpToModule(prog)
  val shrinked = shrink(parsed)
  val withOutComplexOperands = removeComplexOperands(shrinked)
  val explicatedControl = explicateControl(withOutComplexOperands)
  val InstrsWithVar = selectInstructions(explicatedControl)
  // val (instrsWithHome, stackSpace) = allocateRegisters(InstrsWithVar)
  // val patchedInstrs = patchInstructions(instrsWithHome)
  // // val noOps = eliminateNoOps(patchedInstrs)
  // val finalProg = generatePreludeAndConclusion(patchedInstrs, stackSpace)
  // finalProg
  ???
}

def readFile(path: Path): String = {
  val source = scala.io.Source.fromFile(path.toFile)
  try source.getLines.mkString("\n")
  finally source.close()
}
