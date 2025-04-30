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
import compiler.Stmt.*
import compiler.Expr.*
import scala.io.StdIn

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

def readExpression(sexpr: SExp): Expr = sexpr match
  case Number(n) => Constant(n)
  case Node(Symbol("Call") :: Symbol(name) :: argsNode :: Nil) =>
    val args = argsNode match
      case Node(elements) => elements.map(readExpression)
      case _ => sys error "invalid argument list" + argsNode
    Call(name, args)
  // UnaryOp
  case Node(List(Symbol("-"), e)) => 
    UnaryOp(UnaryOperator.USub, readExpression(e))
  // Additon
  case Node(List(l, Symbol("+"), r)) => 
    BinaryOp(readExpression(l), BinaryOperator.Add, readExpression(r))
  // Substraction
  case Node(List(l, Symbol("-"), r)) => 
    BinaryOp(readExpression(l), BinaryOperator.Sub, readExpression(r))
  case Symbol(name) => 
    Call(name, Nil)
  case Node(List(inner)) => readExpression(inner)
  case other => sys error "invalid expression" + other

def readStatement(sexpr: SExp): Stmt = sexpr match
  case Node(List(Symbol("Expr"), e)) => 
    ExprStmt(readExpression(e))
  case other => sys error "invalid statement: " + other
 
def readModule(sexpr: SExp): Module = sexpr match
  case Node(Symbol("Module") :: stmtSexps) =>
    val stmts = stmtSexps.map(readStatement)
    Module(stmts)
  case other => 
    sys error "invalid Module: " + other 


def evalExpr(e: Expr): Long = e match
  case Constant(n) => n
  case UnaryOp(UnaryOperator.USub, e) => - evalExpr(e)
  case BinaryOp(left, BinaryOperator.Add, right) => evalExpr(left) + evalExpr(right)
  case BinaryOp(left, BinaryOperator.Sub, right) => evalExpr(left) - evalExpr(right)
  case Call("print", args) =>
    val evArgs = args.map(evalExpr)
    println(evArgs.mkString(" ")) 
    0L
  case Call("input_int", Nil) => StdIn.readInt().toLong
  case Call(f, _) => sys error "unknown function call " + f

def partialEval(e: Expr): Expr = e match
  case c @ Constant(n) => c
  case i @ Call("input_int", Nil) => i
  case UnaryOp(UnaryOperator.USub, e) =>
    partialEval(e) match
        case Constant(n) => Constant(-n)
        case expr        => UnaryOp(UnaryOperator.USub, expr)
  case BinaryOp(left, BinaryOperator.Add, right) =>
    (partialEval(left), partialEval(right)) match
        case (Constant(a), Constant(b)) => Constant(a + b)
        case (left, right)              => BinaryOp(left, BinaryOperator.Add, right)
  case BinaryOp(left, BinaryOperator.Sub, right) =>
    (partialEval(left), partialEval(right)) match
        case (Constant(a), Constant(b)) => Constant(a - b)
        case (left, right)              => BinaryOp(left, BinaryOperator.Sub, right)
  case other => other



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
