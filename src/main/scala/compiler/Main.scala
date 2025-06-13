package compiler

import lang.SExp
import lang.SExp.*
import lang.parse
import x86.Program
import x86.format
import x86.assemble

import java.nio.file.{Path, Paths}
import scala.io.StdIn
import compiler.RegisterAllocation.*
import analysis.*

@main
def main(): Unit =
  // val path = "examples/print_42.lang"
  // compile(Paths.get(path))
  val parsed = parse(
    """
x = 0
while (x < 3) {
  print(x)
  x = x + 1
}
  """
  )

  val prog = selectInstructions(
    explicateControl(
      simplifyModule(shrinkModule(LIfReader.fromSExpToModule(parsed)))
    )
  )

  val live = uncoverLive(prog)
  println("Live variables: " + live)
  val interGraph = interferenceGraph(live)
  println("Interference Graph: " + interGraph)
  val coloring = dsatur(interGraph, Map.empty)
  println("coloring: " + coloring)
  val homes = homesFor(coloring)
  println("homes: " + homes)
  val assignedHomes = assignHomes(prog, homes)
  println("assignedHomes: " + assignedHomes)

  val graph = basicblockGraph(prog)
  // println("selected instructions: " + prog)
  println("Basic Block Graph: " + graph)
  dumpGraph(
    prog.blocks,
    graph,
    instr => instr.toString,
    "graph.png"
  )

  // println("after parse: " + parsed)
  // val sexped = LIfReader.fromSExpToModule(parsed)
  // println("after sexp: " + sexped)
  // val shrinked = shrinkModule(sexped)
  // println("after shrink: " + shrinked)
  // val removedComplex = simplifyModule(shrinked)
  // println("after removedComplexOperands: " + removedComplex)
  // val controlled = explicateControl(removedComplex)
  // println("after explicateControll: " + controlled)
  // val selctedInstructions = selectInstructions(controlled)
  // println("after selectInstructions: " + selctedInstructions)
  // val (assignedHomes, stackSpace) = allocateRegisters(selctedInstructions)
  // println("after assignHomes: " + assignedHomes)
  // val patchedInstructions = patchInstructions(assignedHomes)
  // println("after patchInstructions: " + patchedInstructions)
  // val finalProg = preludeAndConclusion(patchedInstructions, stackSpace)
  // println("after preludeAndConclusion: " + finalProg)

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
  val (instrsWithHome, stackSpace) = allocateRegisters(InstrsWithVar)
  val patchedInstrs = patchInstructions(instrsWithHome)
  val finalProg = preludeAndConclusion(patchedInstrs, stackSpace)
  finalProg
}

def readFile(path: Path): String = {
  val source = scala.io.Source.fromFile(path.toFile)
  try source.getLines.mkString("\n")
  finally source.close()
}
