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
import scala.io.StdIn
import LMonVar.AtomExpr
import CommonOperators.*

@main
def main(): Unit =
  val program = "1 + (2+2) -8"
  println(simplifyModule(LVarReader.fromSExpToModule(parse(program))))
  // compile(Paths.get(path))

var counter = 0
def freshName(): LVar.Identifier =
  counter += 1
  LVar.Identifier("$tmp$_" + counter)

  /** Helper to extract the atom from an expression.
    *
    * This is used to ensure that the expression is an atom and not a more
    * complex expression.
    */
def extractAtom(expr: LMonVar.Expr): LMonVar.Atom = expr match
  case AtomExpr(a) => a
  case _           => sys.error("Expected an AtomExpr, but got: " + expr)

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
def simplifyExpr(
    exp: LVar.Expr
): (List[LMonVar.AssignStmt], LMonVar.Expr) = exp match
  case LVar.Constant(n) => (Nil, AtomExpr(LMonVar.Constant(n)))
  case LVar.Variable(LVar.Identifier(name)) =>
    (Nil, AtomExpr(LMonVar.Variable(LMonVar.Identifier(name))))
  case LVar.UnaryOp(op, e) =>
    val (assignments, atomExpr) = simplifyExpr(e)
    val LVar.Identifier(tmp) = freshName()
    val newAssignment: LMonVar.AssignStmt =
      LMonVar.AssignStmt(
        LMonVar.Identifier(tmp),
        LMonVar.UnaryOp(
          op,
          extractAtom(
            atomExpr
          ) // Ensure we only use the atom from the expression
        )
      )
    val extendedAssignments = assignments :+ newAssignment
    (
      extendedAssignments,
      AtomExpr(LMonVar.Variable(LMonVar.Identifier(tmp)))
    )
  case LVar.BinaryOp(op, lhs, rhs) =>
    val (assignmentsLeft, atomExprLeft) = simplifyExpr(lhs)
    val (assignmentsRight, atomExprRight) = simplifyExpr(rhs)
    val LVar.Identifier(tmp) = freshName()
    val newAssignment: LMonVar.AssignStmt = LMonVar.AssignStmt(
      LMonVar.Identifier(tmp),
      LMonVar.BinaryOp(
        op,
        extractAtom(atomExprLeft),
        extractAtom(atomExprRight)
      )
    )
    val extendedAssignments =
      assignmentsLeft ++ assignmentsRight :+ newAssignment
    (
      extendedAssignments,
      AtomExpr(LMonVar.Variable(LMonVar.Identifier(tmp)))
    )
  case LVar.Call(LVar.Identifier(name), args) =>
    val simplified = args.map(simplifyExpr)
    val assignments = simplified.flatMap(_._1)
    val atomExprArgs = simplified.map(_._2)
    val LVar.Identifier(tmp) = freshName()
    val newAssignment: LMonVar.AssignStmt =
      LMonVar.AssignStmt(
        LMonVar.Identifier(tmp),
        LMonVar.Call(
          LMonVar.Identifier(name),
          atomExprArgs.map(extractAtom)
        )
      )
    (
      assignments :+ newAssignment,
      AtomExpr(LMonVar.Variable(LMonVar.Identifier(tmp)))
    )
  case LVar.Identifier(name) => sys error "can not simplify identifiers"

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
def simplifyStmt(stmt: LVar.Stmt): List[LMonVar.Stmt] = stmt match
  case LVar.ExprStmt(e) =>
    val (assignments, expr) = simplifyExpr(e)
    assignments :+ LMonVar.ExprStmt(expr)
  case LVar.PrintStmt(e) =>
    val (assignments, atomExpr) = simplifyExpr(e)
    assignments :+ LMonVar.PrintStmt(extractAtom(atomExpr))
  case LVar.AssignStmt(LVar.Identifier(name), e) =>
    val (assignments, expr) = simplifyExpr(e)
    assignments :+ LMonVar.AssignStmt(LMonVar.Identifier(name), expr)

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
def simplifyModule(module: LVar.Module): LMonVar.Module =
  val simplifiedStmts = module.stmts.flatMap(simplifyStmt)
  LMonVar.Module(simplifiedStmts)

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
