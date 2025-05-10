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

import java.nio.file.{Path, Paths}
import AST.Stmt.*
import AST.Expr.*
import AST.Expr
import AST.Module
import scala.io.StdIn
import AST.UnaryOperator
import compiler.AST.BinaryOperator
import compiler.AST.Stmt

@main
def main(): Unit =
  val program = "1 + (2+2) -8"
  println(simplifyModule(LVarReader.fromSExpToModule(parse(program))))
  // compile(Paths.get(path))

var counter = 0
def freshName(): String =
  counter += 1
  "$tmp$_" + counter

/** Simplifies a potentially complex expression by introducing temporary
  * variables.
  *
  * This function recursively transforms nested expressions into a flat sequence
  * of assignments, where each subexpression is computed and stored in a fresh
  * variable. The resulting expression is guaranteed to be in a simple form
  * (i.e., only variables and constants as operands).
  *
  * @param exp
  *   the expression to simplify
  * @return
  *   a pair consisting of:
  *   - a list of assignment statements computing intermediate results
  *   - the final simplified expression using only simple operands
  */
def simplifyExpr(exp: Expr): (List[AssignStmt], Expr) = exp match
  case Constant(n)    => (Nil, exp)
  case Variable(name) => (Nil, exp)
  case UnaryOp(op, e) =>
    val (assignments, simpleExpr) = simplifyExpr(e)
    val tmp = freshName()
    val newAssignment: AssignStmt =
      AssignStmt(tmp, UnaryOp(op, simpleExpr))
    val extendedAssignments = assignments :+ newAssignment
    (extendedAssignments, Variable(tmp))
  case BinaryOp(op, lhs, rhs) =>
    val (assignmentsLeft, simpleExprLeft) = simplifyExpr(lhs)
    val (assignmentsRight, simpleExprRight) = simplifyExpr(rhs)
    val tmp = freshName()
    val newAssignment: AssignStmt = AssignStmt(
      tmp,
      BinaryOp(op, simpleExprLeft, simpleExprRight)
    )
    val extendedAssignments =
      assignmentsLeft ++ assignmentsRight :+ newAssignment
    (extendedAssignments, Variable(tmp))
  case Call(name, args) =>
    val simplified = args.map(simplifyExpr)
    val assignments = simplified.flatMap(_._1)
    val simpleArgs = simplified.map(_._2)
    val tmp = freshName()
    val newAssignment: AssignStmt = AssignStmt(tmp, Call(name, simpleArgs))
    (assignments :+ newAssignment, Variable(tmp))

  /** Simplifies all expressions within a statement by extracting complex
    * subexpressions.
    *
    * This function applies `simplifyExpr` to each expression inside the
    * statement (whether it's an assignment, print, or standalone expression).
    * Any intermediate computations are lifted into separate assignment
    * statements.
    *
    * @param stmt
    *   the statement to simplify
    * @return
    *   a list of simplified statements, including any generated temporary
    *   assignments
    */
def simplifyStmt(stmt: Stmt): List[Stmt] = stmt match
  case ExprStmt(e) =>
    val (assignments, simpleExpr) = simplifyExpr(e)
    assignments :+ ExprStmt(simpleExpr)
  case PrintStmt(e) =>
    val (assignments, simpleExpr) = simplifyExpr(e)
    assignments :+ PrintStmt(simpleExpr)
  case AssignStmt(name, e) =>
    val (assignments, simpleExpr) = simplifyExpr(e)
    assignments :+ AssignStmt(name, simpleExpr)

  /** Simplifies all statements in a module by flattening expressions
    * throughout.
    *
    * Applies `simplifyStmt` to each statement in the module and combines all
    * resulting statements into a new, fully simplified module.
    *
    * @param module
    *   the module to simplify
    * @return
    *   a new module with all expressions simplified and lifted into flat
    *   statements
    */
def simplifyModule(module: Module): Module =
  val simplifiedStmts = module.stmts.flatMap(simplifyStmt)
  Module(simplifiedStmts)

def compile(input: Path): Path = {
  val basename = input.getFileName.toString.replace(".lang", "")
  val source = readFile(input)
  val target = replaceMeWithTheActualCompilation(parse(source))
  assemble(target, basename)
}

def replaceMeWithTheActualCompilation(prog: SExp): Program = prog match {
  case Node(
        List(
          Symbol("Module"),
          Node(
            List(
              Node(
                List(
                  Symbol("Expr"),
                  Node(
                    List(
                      Symbol("Call"),
                      Node(List(Symbol("Variable"), Symbol("print"))),
                      Node(List(Node(List(Symbol("Constant"), Number(42)))))
                    )
                  )
                )
              )
            )
          )
        )
      ) =>
    Program(
      Map(
        "main" -> List(
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
        )
      )
    )
  case _ => ???
}

def readFile(path: Path): String = {
  val source = scala.io.Source.fromFile(path.toFile)
  try source.getLines.mkString("\n")
  finally source.close()
}
