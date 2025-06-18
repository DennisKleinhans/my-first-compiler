package passes

import compiler.{LCore, CommonNodes}
import CommonNodes.*
import LCore.Stmt
import LCore.Expr

object Shrink {

  /** Shrinks the LIf module by simplifying its statements and expressions.
    *
    * @param module
    * @return
    */
  def shrink(module: LCore.Module): LCore.Module =
    LCore.Module(module.stmts.map(shrinkStmt))

  /** Shrinks a single statement by simplifying its expressions.
    *
    * @param stmt
    * @return
    */
  def shrinkStmt(stmt: LCore.Stmt): LCore.Stmt = stmt match
    case LCore.ExprStmt(e)       => LCore.ExprStmt(shrinkExpr(e))
    case LCore.PrintStmt(e)      => LCore.PrintStmt(shrinkExpr(e))
    case LCore.AssignStmt(id, e) => LCore.AssignStmt(id, shrinkExpr(e))
    case LCore.IfStmt(cond, thenBranch, elseBranch) =>
      LCore.IfStmt(
        shrinkExpr(cond),
        thenBranch.map(shrinkStmt),
        elseBranch.map(shrinkStmt)
      )
    case LCore.WhileStmt(cond, body) =>
      LCore.WhileStmt(shrinkExpr(cond), body.map(shrinkStmt))

  /** Shrinks an expression by simplifying its structure and removing
    * unnecessary complexity by replacing the `and` and `or` operators with if
    * expressions.
    *
    * @param e
    *   the expression to shrink
    * @return
    *   a simplified version of the expression
    */
  def shrinkExpr(e: LCore.Expr): LCore.Expr = e match
    case LCore.UnaryNumericOp(op, e) =>
      LCore.UnaryNumericOp(op, shrinkExpr(e))
    case LCore.UnaryLogicOp(op, e) => LCore.UnaryLogicOp(op, shrinkExpr(e))
    case LCore.BinaryNumericOp(op, lhs, rhs) =>
      LCore.BinaryNumericOp(op, shrinkExpr(lhs), shrinkExpr(rhs))
    case LCore.BinaryLogicOp(BinaryLogicOperator.And, e1, e2) =>
      LCore.IfExpr(shrinkExpr(e1), shrinkExpr(e2), LCore.ConstantBool(false))
    case LCore.BinaryLogicOp(BinaryLogicOperator.Or, e1, e2) =>
      LCore.IfExpr(shrinkExpr(e1), LCore.ConstantBool(true), shrinkExpr(e2))
    case Expr.Compare(cmp, lhs, rhs) =>
      LCore.Compare(cmp, shrinkExpr(lhs), shrinkExpr(rhs))
    case Expr.IfExpr(condExpr, thenExpr, elseExpr) =>
      LCore.IfExpr(
        shrinkExpr(condExpr),
        shrinkExpr(thenExpr),
        shrinkExpr(elseExpr)
      )
    case _ => e
}
