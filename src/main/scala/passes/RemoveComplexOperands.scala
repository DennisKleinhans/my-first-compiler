package passes

import compiler.CommonNodes.*
import compiler.{LCore, LMon}
import LMon.Expr
import LMon.Expr.AtomExpr
import compiler.CommonNodes.Atom.Constant

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
  def removeComplexOperands(
      exp: LCore.Expr,
      gen: NameGenerator
  ): (List[LMon.Stmt], LMon.Expr) = exp match
    case LCore.Constant(n) => (Nil, Expr.AtomExpr(Atom.Constant(n)))

    case LCore.ConstantBool(b) => (Nil, Expr.AtomExpr(Atom.ConstantBool(b)))

    case LCore.Variable(Identifier(name)) =>
      (Nil, AtomExpr(Atom.Variable(Identifier(name))))

    case LCore.UnaryNumericOp(op, e) =>
      val (assignments, atomExpr) = removeComplexOperands(e, gen)
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
      val (assignmentsLeft, atomExprLeft) = removeComplexOperands(lhs, gen)
      val (assignmentsRight, atomExprRight) = removeComplexOperands(rhs, gen)
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
      val (assignmentsE1, atomExprE1) = removeComplexOperands(e1, gen)
      val (assignmentsE2, atomExprE2) = removeComplexOperands(e2, gen)
      val simpleCompare = LMon.Compare(
        cmp,
        extractAtom(atomExprE1),
        extractAtom(atomExprE2)
      )

      (assignmentsE1 ++ assignmentsE2, simpleCompare)

    case LCore.UnaryLogicOp(op, e) =>
      val (assignments, simplifiedExpr) = removeComplexOperands(e, gen)
      (assignments, LMon.UnaryLogicOp(op, simplifiedExpr))

    case LCore.IfExpr(test, thn, els) =>
      val (condAssignments, simpleCondExpr) = removeComplexOperands(test, gen)
      val (thenAssignments, simpleThenExpr) = removeComplexOperands(thn, gen)
      val (elseAssignments, simpleElseExpr) = removeComplexOperands(els, gen)

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

    case LCore.Tuple(elements) =>
      // simplify all tuple elements
      val (assignments, simpleElements) = elements.foldLeft(
        (List.empty[LMon.Stmt], List.empty[LMon.Expr])
      ) { case ((assignAcc, atomAcc), expr) =>
        val (assigns, atom) = removeComplexOperands(expr, gen)
        (assignAcc ++ assigns, atomAcc :+ atom)
      }

      // variable to store the pointer
      val ptrId = gen.freshName()
      val ptrVar = Atom.Variable(ptrId)

      // calculate size of the tuple + tag for metadata like length
      val numElements = simpleElements.size
      val sizeBytes = (numElements + 1) * 8

      // allocate the needed memory and assign the returned pointer to the pointer variable
      val allocAssign: LMon.AssignStmt =
        LMon.AssignStmt(ptrId, LMon.Allocate(sizeBytes))

      // store the length tag at the first 8 bytes where the pointer points to
      val tagLenStore = LMon.StoreStmt(ptrVar, 0, Constant(numElements))

      // store every tuple element at it offset starting at the pointer as the base
      val elementStores =
        simpleElements.zipWithIndex.map { case (atomExpr, i) =>
          LMon.StoreStmt(ptrVar, 8 * (i + 1), extractAtom(atomExpr))

        }
      (
        allocAssign :: tagLenStore :: assignments ++ elementStores,
        AtomExpr(Atom.Variable(ptrId))
      )

    case LCore.TupleProjection(tuple, index) =>
      val (assignments, tupleAtom) = removeComplexOperands(tuple, gen)
      val ptrId = gen.freshName()
      val ptrVar = Atom.Variable(ptrId)

      // just use the index directly here, because the projections start with tuple._1
      val loadAssign = LMon.AssignStmt(
        ptrId,
        LMon.Load(extractAtom(tupleAtom), 8 * (index + 1))
      )

      (assignments :+ loadAssign, AtomExpr(ptrVar))

    case LCore.TupleLen(tuple) =>
      val (assignments, tupleAtom) = removeComplexOperands(tuple, gen)
      val ptrId = gen.freshName()
      val ptrVar = Atom.Variable(ptrId)

      // access the length tag
      val loadAssign =
        LMon.AssignStmt(ptrId, LMon.Load(extractAtom(tupleAtom), 0))

      (assignments :+ loadAssign, AtomExpr(ptrVar))

    case LCore.Call(name, args) =>
      val (assignments, simpleArgs) = args.foldLeft(
        (List.empty[LMon.Stmt], List.empty[Atom])
      ) { case ((assignAcc, atomAcc), expr) =>
        val (assigns, atom) = removeComplexOperands(expr, gen)
        (assignAcc ++ assigns, atomAcc :+ extractAtom(atom))
      }
      (assignments, LMon.Call(name, simpleArgs))

    case e @ LCore.BinaryLogicOp(_, _, _) =>
      sys error s"rached BinaryLogicOp ($e), but this should not happen, because this would be removed in the shrink pass"

    /** Simplifies all expressions within a statement by extracting complex
      * subexpressions.
      *
      * This function applies `removeComplexOperands` to each expression inside
      * the statement (whether it's an assignment, print, or standalone
      * expression). Any intermediate computations are lifted into separate
      * assignment statements.
      *
      * @param stmt
      *   the statement to simplify
      * @return
      *   a list of simplified statements, including any generated temporary
      *   assignments
      */
  def removeComplexOperands(
      stmt: LCore.Stmt,
      gen: NameGenerator
  ): List[LMon.Stmt] =
    stmt match
      case LCore.ExprStmt(e) =>
        val (assignments, expr) = removeComplexOperands(e, gen)
        assignments :+ LMon.ExprStmt(expr)

      case LCore.PrintStmt(e) =>
        val (assignments, expr) = removeComplexOperands(e, gen)
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
        val (assignments, expr) = removeComplexOperands(e, gen)
        assignments :+ LMon.AssignStmt(Identifier(name), expr)

      case LCore.IfStmt(test, thn, els) =>
        val (condAssignments, simpleCondExpr) = removeComplexOperands(test, gen)
        val simplifiedThenStmts = thn.flatMap(removeComplexOperands(_, gen))
        val simplifiedElseStmts = els.flatMap(removeComplexOperands(_, gen))

        condAssignments :+ LMon.IfStmt(
          simpleCondExpr,
          simplifiedThenStmts,
          simplifiedElseStmts
        )

      case LCore.WhileStmt(test, body) =>
        val (condAssignments, simpleCondExpr) = removeComplexOperands(test, gen)
        val simplifiedBodyStmts = body.flatMap(removeComplexOperands(_, gen))

        // wrap the condition in a Begin to make sure also complex conditions (e.g x-1 < 3) are evaluated as a block and not just the last expression
        List(
          LMon.WhileStmt(
            LMon.Begin(condAssignments, simpleCondExpr),
            simplifiedBodyStmts
          )
        )

      case LCore.ReturnStmt(e) =>
        val (assignments, expr) = removeComplexOperands(e, gen)
        expr match
          case AtomExpr(a) =>
            assignments :+ LMon.ReturnStmt(a)
          case other =>
            val tmpIdentifier = gen.freshName()
            assignments ++ List(
              LMon.AssignStmt(tmpIdentifier, other),
              LMon.ReturnStmt(Atom.Variable(tmpIdentifier))
            )

      /** Simplifies all statements in a module by flattening expressions
        * throughout.
        *
        * Applies `removeComplexOperands` to each statement in the module and
        * combines all resulting statements into a new, fully simplified module.
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
    val funDefs = module.funDefs.map { funDef =>
      LMon.FunctionDef(
        funDef.name,
        funDef.params,
        funDef.body.flatMap(removeComplexOperands(_, gen))
      )
    }
    LMon.Module(funDefs)
}
