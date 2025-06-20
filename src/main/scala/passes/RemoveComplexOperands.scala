package passes

import compiler.CommonNodes.*
import compiler.{LCore, LMon}
import LMon.Expr
import LMon.Expr.AtomExpr

object RemoveComplexOperands {

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
    * This is used to ensure that the expression is an atom and not a more
    * complex expression.
    */
  def extractAtom(expr: LMon.Expr): Atom = expr match
    case AtomExpr(a) => a
    case _           => sys.error("Expected an AtomExpr, but got: " + expr)

  /** Simplifies a potentially complex expression by introducing temporary
    * variables.
    *
    * This function recursively transforms nested expressions into a flat
    * sequence of assignments, where each subexpression is computed and stored
    * in a fresh variable. The resulting expression is guaranteed to be in a
    * simple form (i.e., only variables and constants as operands).
    *
    * @param exp
    *   the expression to simplify
    * @return
    *   a pair consisting of:
    *   - a list of assignment statements computing intermediate results
    *   - the final simplified expression using only simple operands
    */
  def simplifyExpr(
      exp: LCore.Expr,
      gen: NameGenerator
  ): (List[LMon.AssignStmt], LMon.Expr) = exp match
    case LCore.Constant(n) => (Nil, Expr.AtomExpr(Atom.Constant(n)))

    case LCore.ConstantBool(b) => (Nil, Expr.AtomExpr(Atom.ConstantBool(b)))

    case LCore.Variable(Identifier(name)) =>
      (Nil, AtomExpr(Atom.Variable(Identifier(name))))

    case LCore.UnaryNumericOp(op, e) =>
      val (assignments, atomExpr) = simplifyExpr(e, gen)
      val tmpIdentifier = gen.freshName()
      val newAssignment: LMon.AssignStmt =
        LMon.AssignStmt(
          tmpIdentifier,
          LMon.UnaryNumericOp(
            op,
            extractAtom(
              atomExpr
            )
          )
        )
      val extendedAssignments = assignments :+ newAssignment
      (
        extendedAssignments,
        AtomExpr(Atom.Variable(tmpIdentifier))
      )

    case LCore.BinaryNumericOp(op, lhs, rhs) =>
      val (assignmentsLeft, atomExprLeft) = simplifyExpr(lhs, gen)
      val (assignmentsRight, atomExprRight) = simplifyExpr(rhs, gen)
      val tmpIdentifier = gen.freshName()
      val newAssignment: LMon.AssignStmt = LMon.AssignStmt(
        tmpIdentifier,
        LMon.BinaryNumericOp(
          op,
          extractAtom(atomExprLeft),
          extractAtom(atomExprRight)
        )
      )
      val extendedAssignments =
        assignmentsLeft ++ assignmentsRight :+ newAssignment
      (
        extendedAssignments,
        AtomExpr(Atom.Variable(tmpIdentifier))
      )

    case LCore.Compare(cmp, e1, e2) =>
      val (assignmentsE1, atomExprE1) = simplifyExpr(e1, gen)
      val (assignmentsE2, atomExprE2) = simplifyExpr(e2, gen)
      val simpleCompare = LMon.Compare(
        cmp,
        extractAtom(atomExprE1),
        extractAtom(atomExprE2)
      )

      (assignmentsE1 ++ assignmentsE2, simpleCompare)

    case LCore.UnaryLogicOp(op, e) =>
      val (assignments, simplifiedExpr) = simplifyExpr(e, gen)
      (assignments, LMon.UnaryLogicOp(op, simplifiedExpr))

    case LCore.IfExpr(test, thn, els) =>
      val (condAssignments, simpleCondExpr) = simplifyExpr(test, gen)
      val (thenAssignments, simpleThenExpr) = simplifyExpr(thn, gen)
      val (elseAssignments, simpleElseExpr) = simplifyExpr(els, gen)

      val simpleIfExpr =
        LMon.IfExpr(
          simpleCondExpr,
          LMon.Begin(thenAssignments, simpleThenExpr),
          LMon.Begin(elseAssignments, simpleElseExpr)
        )

      (condAssignments, simpleIfExpr)

    case LCore.ReadIntCall =>
      val tmpIdentifier = gen.freshName()
      val newAssignment: LMon.AssignStmt = LMon.AssignStmt(
        tmpIdentifier,
        LMon.ReadIntCall
      )
      (
        List(newAssignment),
        AtomExpr(Atom.Variable(tmpIdentifier))
      )

    case e @ LCore.BinaryLogicOp(_, _, _) =>
      sys error s"rached BinaryLogicOp ($e), but this should not happen, because this would be removed in the shrink pass"

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
      stmt: LCore.Stmt,
      gen: NameGenerator
  ): List[LMon.Stmt] =
    stmt match
      case LCore.ExprStmt(e) =>
        val (assignments, expr) = simplifyExpr(e, gen)
        assignments :+ LMon.ExprStmt(expr)

      case LCore.PrintStmt(e) =>
        val (assignments, expr) = simplifyExpr(e, gen)
        expr match
          case AtomExpr(a) =>
            assignments :+ LMon.PrintStmt(a)
          case other =>
            val tmpIdentifier = gen.freshName()
            assignments ++ List(
              LMon.AssignStmt(tmpIdentifier, other),
              LMon.PrintStmt(Atom.Variable(tmpIdentifier))
            )

      case LCore.AssignStmt(Identifier(name), e) =>
        val (assignments, expr) = simplifyExpr(e, gen)
        assignments :+ LMon.AssignStmt(Identifier(name), expr)

      case LCore.IfStmt(test, thn, els) =>
        val (condAssignments, simpleCondExpr) = simplifyExpr(test, gen)
        val simplifiedThenStmts = thn.flatMap(simplifyStmt(_, gen))
        val simplifiedElseStmts = els.flatMap(simplifyStmt(_, gen))

        condAssignments :+ LMon.IfStmt(
          simpleCondExpr,
          simplifiedThenStmts,
          simplifiedElseStmts
        )

      case LCore.WhileStmt(test, body) =>
        // 1) Test vereinfachen, aber Zuweisungen *nicht* vorziehen
        val (condAssignments, simpleCondExpr) = simplifyExpr(test, gen)

        // 2) Körper wie gehabt rekursiv simplifizieren
        val simplifiedBodyStmts = body.flatMap(simplifyStmt(_, gen))

        // 3) Nur noch eine WhileStmt zurückgeben – ohne condAssignments
        List(
          LMon.WhileStmt(
            LMon.Begin(condAssignments, simpleCondExpr),
            simplifiedBodyStmts
          )
        )

      /** Simplifies all statements in a module by flattening expressions
        * throughout.
        *
        * Applies `simplifyStmt` to each statement in the module and combines
        * all resulting statements into a new, fully simplified module.
        *
        * @param module
        *   the module to simplify
        * @return
        *   a new module with all expressions simplified and lifted into flat
        *   statements
        */
  def removeComplexOperands(
      module: LCore.Module,
      gen: NameGenerator = NameGenerator()
  ): LMon.Module =
    val simplifiedStmts = module.stmts.flatMap(simplifyStmt(_, gen))
    LMon.Module(simplifiedStmts)
}
