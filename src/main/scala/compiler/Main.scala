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
  val parsed = parse("if (true) {1} else {2}")
  val sexped = LIfReader.fromSExpToModule(parsed)
  println(sexped)

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
  val withOutComplexOperands = simplifyModule(parsed)
  val InstrsWithVar = selectInstructions(withOutComplexOperands)
  val (instrsWithHome, stackSpace) = assignHomes(InstrsWithVar)
  val patchedInstrs = patchInstructions(instrsWithHome)
  val finalProg = preludeAndConclusion(patchedInstrs, stackSpace)

  x86.Program(Map("main" -> finalProg))
}

def readFile(path: Path): String = {
  val source = scala.io.Source.fromFile(path.toFile)
  try source.getLines.mkString("\n")
  finally source.close()
}
