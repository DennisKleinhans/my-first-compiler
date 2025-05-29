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
  val sexp = Node(
      List(
        Symbol("Expr"),
        Node(
          List(
            Symbol("Call"),
            Node(List(Symbol("Variable"), Symbol("print"))),
            Node(
              List(
                Node(
                  List(
                    Symbol("Binary"),
                    Symbol("Add"),
                    Node(List(Symbol("Variable"), Symbol("x"))),
                    Node(List(Symbol("Constant"), Number(2)))
                  )
                )
              )
            )
          )
        )
      )
    )
  val parsed = LVarReader.fromSExpToStmt(sexp)
  println(parsed)

def compile(input: Path): Path = {
  val basename = input.getFileName.toString.replace(".lang", "")
  val source = readFile(input)
  val sexped = parse(source)
  val target = transformTox86(sexped)
  assemble(target, basename)
}

def transformTox86(prog: SExp): Program = {
  val parsed = LVarReader.fromSExpToModule(prog)
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
