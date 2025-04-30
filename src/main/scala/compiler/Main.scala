package compiler

import lang.SExp
import lang.SExp.*
import lang.parse
import x86.Instr.*
import x86.Arg.*
import x86.Reg.*
import x86.Program
import x86.format
import x86.assemble

import java.nio.file.{ Path, Paths }

@main
def main(path: String): Unit =
  compile(Paths.get(path))


enum Expr:
  case Constant(n: Long)
  case UnaryOp(op: UnaryOperator, e: Expr)
  case BinaryOp(left: Expr, op: BinaryOperator, right: Expr)
  case Call(name: String, args: List[Expr])

enum UnaryOperator:
  case USub

enum BinaryOperator:
  case Add
  case Sub

enum Stmt:
  case ExprStmt(e: Expr)

case class Module(stmts: List[Stmt])


def compile(input: Path): Path = {
  val basename = input.getFileName.toString.replace(".lang", "")
  val source = readFile(input)
  val target = replaceMeWithTheActualCompilation(parse(source))
  assemble(target, basename)
}

def replaceMeWithTheActualCompilation(prog: SExp): Program = prog match {
  case Node(List(
      Symbol("Module"),
      Node(List(Node(List(
        Symbol("Expr"),
        Node(List(
          Symbol("Call"),
          Node(List(Symbol("Variable"), Symbol("print"))),
          Node(List(Node(List(Symbol("Constant"), Number(42)))))
        ))
      ))))
    )) =>
    Program(Map("main" -> List(
      PushQ(Register(Rbp)),
      MovQ(Register(Rsp), Register(Rbp)),
      SubQ(Immediate(16), Register(Rsp)),
      MovQ(Immediate(42), Register(Rax)),
      MovQ(Register(Rax), Register(Rdi)),
      CallQ("print_int", 1),
      MovQ(Immediate(0), Register(Rax)),
      AddQ(Immediate(16), Register(Rsp)),
      PopQ(Register(Rbp)),
      RetQ
    )))
  case _ => ???
}

def readFile(path: Path): String = {
  val source = scala.io.Source.fromFile(path.toFile)
  try source.getLines.mkString("\n")
  finally source.close()
}
