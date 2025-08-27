package passes

import compiler.{LCore, CommonNodes}
import CommonNodes.*
import LCore.Stmt
import LCore.Expr
import compiler.LCore.Stmt.ReturnStmt
import compiler.LCore.Expr.Constant

object Shrink {

  /** Shrinks the LCore module by simplifying its statements and expressions.
    *
    * @param module
    * @return
    */
  def shrink(module: LCore.Module): LCore.Module =
    val shrunkStmts = module.stmts.map(shrink)
    // pack the stmts into a main function definition
    val mainFunDef = LCore.FunctionDef("main", Nil, shrunkStmts :+ ReturnStmt(Constant(0)))
    LCore.Module(mainFunDef :: module.funDefs, Nil)

  /** Shrinks a single statement by simplifying its expressions.
    *
    * @param stmt
    * @return
    */
  def shrink(stmt: LCore.Stmt): LCore.Stmt = stmt match
    case LCore.ExprStmt(e)       => LCore.ExprStmt(shrinkExpr(e))
    case LCore.PrintStmt(e)      => LCore.PrintStmt(shrinkExpr(e))
    case LCore.AssignStmt(id, e) => LCore.AssignStmt(id, shrinkExpr(e))
    case LCore.IfStmt(cond, thenBranch, elseBranch) =>
      LCore.IfStmt(
        shrinkExpr(cond),
        thenBranch.map(shrink),
        elseBranch.map(shrink)
      )
    case LCore.WhileStmt(cond, body) =>
      LCore.WhileStmt(shrinkExpr(cond), body.map(shrink))
    case LCore.ReturnStmt(e) => LCore.ReturnStmt(shrinkExpr(e))

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
