package compiler

import CommonNodes.*
import LMonVar.Expr
import LMonVar.Expr.AtomExpr

/** A generator for unique temporary variable names.
  */
class NameGenerator {
  private var counter = 0

  /** Generates a fresh temporary variable name.
    *
    * @return
    *   a new identifier with a unique name
    */
  def freshName(): Identifier =
    counter += 1
    Identifier("$tmp$_" + counter)
}

/** Helper to extract the atom from an expression.
  *
  * This is used to ensure that the expression is an atom and not a more complex
  * expression.
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
    exp: LVar.Expr,
    gen: NameGenerator = NameGenerator()
): (List[LMonVar.AssignStmt], LMonVar.Expr) = exp match
  case LVar.Constant(n) => (Nil, Expr.AtomExpr(LMonVar.Constant(n)))
  case LVar.Variable(Identifier(name)) =>
    (Nil, AtomExpr(LMonVar.Variable(Identifier(name))))
  case LVar.UnaryOp(op, e) =>
    val (assignments, atomExpr) = simplifyExpr(e, gen)
    val Identifier(tmp) = gen.freshName()
    val newAssignment: LMonVar.AssignStmt =
      LMonVar.AssignStmt(
        Identifier(tmp),
        LMonVar.UnaryOp(
          op,
          extractAtom(
            atomExpr
          )
        )
      )
    val extendedAssignments = assignments :+ newAssignment
    (
      extendedAssignments,
      AtomExpr(LMonVar.Variable(Identifier(tmp)))
    )
  case LVar.BinaryOp(op, lhs, rhs) =>
    val (assignmentsLeft, atomExprLeft) = simplifyExpr(lhs, gen)
    val (assignmentsRight, atomExprRight) = simplifyExpr(rhs, gen)
    val Identifier(tmp) = gen.freshName()
    val newAssignment: LMonVar.AssignStmt = LMonVar.AssignStmt(
      Identifier(tmp),
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
      AtomExpr(LMonVar.Variable(Identifier(tmp)))
    )
  case LVar.Call(Identifier(name), args) =>
    val simplified = args.map(simplifyExpr(_, gen))
    val assignments = simplified.flatMap(_._1)
    val atomExprArgs = simplified.map(_._2)
    val Identifier(tmp) = gen.freshName()
    val newAssignment: LMonVar.AssignStmt =
      LMonVar.AssignStmt(
        Identifier(tmp),
        LMonVar.Call(
          Identifier(name),
          atomExprArgs.map(extractAtom)
        )
      )
    (
      assignments :+ newAssignment,
      AtomExpr(LMonVar.Variable(Identifier(tmp)))
    )

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
def simplifyStmt(stmt: LVar.Stmt, gen: NameGenerator = NameGenerator()): List[LMonVar.Stmt] =
  stmt match
    case LVar.ExprStmt(e) =>
      val (assignments, expr) = simplifyExpr(e, gen)
      assignments :+ LMonVar.ExprStmt(expr)
    case LVar.PrintStmt(e) =>
      val (assignments, atomExpr) = simplifyExpr(e, gen)
      assignments :+ LMonVar.PrintStmt(extractAtom(atomExpr))
    case LVar.AssignStmt(Identifier(name), e) =>
      val (assignments, expr) = simplifyExpr(e, gen)
      assignments :+ LMonVar.AssignStmt(Identifier(name), expr)

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
def simplifyModule(module: LVar.Module, gen: NameGenerator = NameGenerator()): LMonVar.Module =
  val simplifiedStmts = module.stmts.flatMap(simplifyStmt(_, gen))
  LMonVar.Module(simplifiedStmts)
