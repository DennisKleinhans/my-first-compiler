package compiler

import CommonNodes.*
import LMonIf.Expr
import LMonIf.Expr.AtomExpr

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
def extractAtom(expr: LMonIf.Expr): Atom = expr match
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
    exp: LIf.Expr,
    gen: NameGenerator
): (List[LMonIf.AssignStmt], LMonIf.Expr) = exp match
  case LIf.Constant(n) => (Nil, Expr.AtomExpr(LMonIf.Constant(n)))

  case LIf.ConstantBool(b) => (Nil, Expr.AtomExpr(LMonIf.ConstantBool(b)))

  case LIf.Variable(Identifier(name)) =>
    (Nil, AtomExpr(LMonIf.Variable(Identifier(name))))

  case LIf.UnaryNumericOp(op, e) =>
    val (assignments, atomExpr) = simplifyExpr(e, gen)
    val tmpIdentifier = gen.freshName()
    val newAssignment: LMonIf.AssignStmt =
      LMonIf.AssignStmt(
        tmpIdentifier,
        LMonIf.UnaryNumericOp(
          op,
          extractAtom(
            atomExpr
          )
        )
      )
    val extendedAssignments = assignments :+ newAssignment
    (
      extendedAssignments,
      AtomExpr(LMonIf.Variable(tmpIdentifier))
    )

  case LIf.BinaryNumericOp(op, lhs, rhs) =>
    val (assignmentsLeft, atomExprLeft) = simplifyExpr(lhs, gen)
    val (assignmentsRight, atomExprRight) = simplifyExpr(rhs, gen)
    val tmpIdentifier = gen.freshName()
    val newAssignment: LMonIf.AssignStmt = LMonIf.AssignStmt(
      tmpIdentifier,
      LMonIf.BinaryNumericOp(
        op,
        extractAtom(atomExprLeft),
        extractAtom(atomExprRight)
      )
    )
    val extendedAssignments =
      assignmentsLeft ++ assignmentsRight :+ newAssignment
    (
      extendedAssignments,
      AtomExpr(LMonIf.Variable(tmpIdentifier))
    )

  case LIf.Compare(cmp, e1, e2) =>
    val (assignmentsE1, atomExprE1) = simplifyExpr(e1, gen)
    val (assignmentsE2, atomExprE2) = simplifyExpr(e2, gen)
    val tmpIdentifier = gen.freshName()
    val newAssignment: LMonIf.AssignStmt = LMonIf.AssignStmt(
      tmpIdentifier,
      LMonIf.Compare(
        cmp,
        extractAtom(atomExprE1),
        extractAtom(atomExprE2)
      )
    )
    val extendedAssignments =
      assignmentsE1 ++ assignmentsE2 :+ newAssignment
    (
      extendedAssignments,
      AtomExpr(LMonIf.Variable(tmpIdentifier))
    )

  case LIf.UnaryLogicOp(op, e) =>
    val (assignments, simplifiedExpr) = simplifyExpr(e, gen)
    (assignments, LMonIf.UnaryLogicOp(op, simplifiedExpr))

  case LIf.IfExpr(test, thn, els) =>
    val (condAssignments, simpleCondExpr) = simplifyExpr(test, gen)
    val (thenAssignments, simpleThenExpr) = simplifyExpr(thn, gen)
    val (elseAssignments, simpleElseExpr) = simplifyExpr(els, gen)

    val thenBegin = LMonIf.Begin(thenAssignments, simpleThenExpr)
    val elseBegin = LMonIf.Begin(elseAssignments, simpleElseExpr)
    val finalIfExpr = LMonIf.IfExpr(
      simpleCondExpr,
      thenBegin,
      elseBegin
    )
    (condAssignments, finalIfExpr)

  case LIf.ReadIntCall =>
    val tmpIdentifier = gen.freshName()
    val newAssignment: LMonIf.AssignStmt = LMonIf.AssignStmt(
      tmpIdentifier,
      LMonIf.ReadIntCall
    )
    (
      List(newAssignment),
      AtomExpr(LMonIf.Variable(tmpIdentifier))
    )

  case LIf.BinaryLogicOp(_, _, _) =>
    sys error "rached BinaryLogicOp, but this should not happen, because this would be removed in the shrink pass"

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
def simplifyStmt(
    stmt: LIf.Stmt,
    gen: NameGenerator
): List[LMonIf.Stmt] =
  stmt match
    case LIf.ExprStmt(e) =>
      val (assignments, expr) = simplifyExpr(e, gen)
      assignments :+ LMonIf.ExprStmt(expr)

    case LIf.PrintStmt(e) =>
      val (assignments, atomExpr) = simplifyExpr(e, gen)
      assignments :+ LMonIf.PrintStmt(extractAtom(atomExpr))

    case LIf.AssignStmt(Identifier(name), e) =>
      val (assignments, expr) = simplifyExpr(e, gen)
      assignments :+ LMonIf.AssignStmt(Identifier(name), expr)

    case LIf.IfStmt(test, thn, els) =>
      val (condAssignments, simpleCondExpr) = simplifyExpr(test, gen)
      val simplifiedThenStmts = thn.flatMap(simplifyStmt(_, gen))
      val simplifiedElseStmts = els.flatMap(simplifyStmt(_, gen))

      condAssignments :+ LMonIf.IfStmt(
        simpleCondExpr,
        simplifiedThenStmts,
        simplifiedElseStmts
      )

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
def simplifyModule(
    module: LIf.Module,
    gen: NameGenerator = NameGenerator()
): LMonIf.Module =
  val simplifiedStmts = module.stmts.flatMap(simplifyStmt(_, gen))
  LMonIf.Module(simplifiedStmts)
