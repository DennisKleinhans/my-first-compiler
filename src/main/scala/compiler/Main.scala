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
import passes.RegisterAllocation.{allocateRegisters, basicblockGraph}
import optimization.NoOpElimination.eliminateNoOps

@main
def main(): Unit =
  val path = "examples/loop_compound.lang"
  compile(Paths.get(path))
//   val parsed = parse(
//     """
// x = 0
// while (x-1 < 3 ) {
//   print(x)
//   x = x + 1
// }
//   """
//   )

// val prog = selectInstructions(
//   explicateControl(
//     removeComplexOperands(shrink(LIfReader.fromSExpToModule(parsed)))
//   )
// )

// val live = uncoverLive(prog)
// println("Live variables: " + live)
// val interGraph = interferenceGraph(live)
// println("Interference Graph: " + interGraph)
// val coloring = dsatur(interGraph, Map.empty)
// println("coloring: " + coloring)
// val homes = homesFor(coloring)
// println("homes: " + homes)
// val assignedHomes = assignHomes(prog, homes)
// println("assignedHomes: " + assignedHomes)

// val (assignedHomes, stackSpace) = allocateRegisters(prog)
// println("assigend Homes: " + assignedHomes)
// val patchedInstr = patchInstructions(assignedHomes)
// println("patched Instructions: " + patchedInstr)
// // val noOps = eliminateNoOps(patchedInstr)
// val finalProg = generatePreludeAndConclusion(patchedInstr, stackSpace)
// println("final Programm: " + finalProg)

// val graph = basicblockGraph(prog)
// // println("selected instructions: " + prog)
// println("Basic Block Graph: " + graph)
// dumpGraph(
//   prog.blocks,
//   graph,
//   instr => instr.toString,
//   "graph.png"
// )

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
  val parsed = LCoreReader.fromSExpToModule(prog)
  val shrinked = shrink(parsed)
  val withOutComplexOperands = removeComplexOperands(shrinked)
  val explicatedControl = explicateControl(withOutComplexOperands)
  val InstrsWithVar = selectInstructions(explicatedControl)
  val (instrsWithHome, stackSpace) = allocateRegisters(InstrsWithVar)
  val patchedInstrs = patchInstructions(instrsWithHome)
  // val noOps = eliminateNoOps(patchedInstrs)
  val finalProg = generatePreludeAndConclusion(patchedInstrs, stackSpace)
  finalProg
}

def readFile(path: Path): String = {
  val source = scala.io.Source.fromFile(path.toFile)
  try source.getLines.mkString("\n")
  finally source.close()
}
