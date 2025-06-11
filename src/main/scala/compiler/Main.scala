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
  // val path = "examples/print_42.lang"
  // compile(Paths.get(path))
  val parsed = parse(
    "if (5 > 3) {  print(1)} else {  print(0)}"
  )


  val sexped = LIfReader.fromSExpToModule(parsed)
  println("after sexp: " + sexped)
  val shrinked = shrinkModule(sexped)
  println("after shrink: " + shrinked)
  val removedComplex = simplifyModule(shrinked)
  println("after removedComplexOperands: " + removedComplex)
  val controlled = explicateControl(removedComplex)
  println("after explicateControll: " + controlled)
  val selctedInstructions = selectInstructions(controlled)
  println("after selectInstructions: " + selctedInstructions)
  val (assignedHomes, stackSpace) = assignHomes(selctedInstructions)
  println("after assignHomes: " + assignedHomes)
  val patchedInstructions = patchInstructions(assignedHomes)
  println("after patchInstructions: " + patchedInstructions)
  val finalProg = preludeAndConclusion(patchedInstructions, stackSpace)
  println("after preludeAndConclusion: " + finalProg)

def compile(input: Path): Path = {
  val basename = input.getFileName.toString.replace(".lang", "")
  val source = readFile(input)
  val sexped = parse(source)
  val target = transformTox86(sexped)
  assemble(target, basename)
}

def transformTox86(prog: SExp): Program = {
  val parsed = LIfReader.fromSExpToModule(prog)
  val shrinked = shrinkModule(parsed)
  val withOutComplexOperands = simplifyModule(shrinked)
  val explicatedControl = explicateControl(withOutComplexOperands)
  val InstrsWithVar = selectInstructions(explicatedControl)
  val (instrsWithHome, stackSpace) = assignHomes(InstrsWithVar)
  val patchedInstrs = patchInstructions(instrsWithHome)
  val finalProg = preludeAndConclusion(patchedInstrs, stackSpace)
  finalProg
}

def readFile(path: Path): String = {
  val source = scala.io.Source.fromFile(path.toFile)
  try source.getLines.mkString("\n")
  finally source.close()
}
