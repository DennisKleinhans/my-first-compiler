package compiler

import compiler.LWhile.Expr
import CommonNodes.*
import compiler.LWhile.Stmt

/** Shrinks the LIf module by simplifying its statements and expressions.
  *
  * @param module
  * @return
  */
def shrinkModule(module: LWhile.Module): LWhile.Module =
  LWhile.Module(module.stmts.map(shrinkStmt))

/** Shrinks a single statement by simplifying its expressions.
  *
  * @param stmt
  * @return
  */
def shrinkStmt(stmt: LWhile.Stmt): LWhile.Stmt = stmt match
  case LWhile.ExprStmt(e)       => LWhile.ExprStmt(shrinkExpr(e))
  case LWhile.PrintStmt(e)      => LWhile.PrintStmt(shrinkExpr(e))
  case LWhile.AssignStmt(id, e) => LWhile.AssignStmt(id, shrinkExpr(e))
  case LWhile.IfStmt(cond, thenBranch, elseBranch) =>
    LWhile.IfStmt(
      shrinkExpr(cond),
      thenBranch.map(shrinkStmt),
      elseBranch.map(shrinkStmt)
    )
  case LWhile.WhileStmt(cond, body) =>
    LWhile.WhileStmt(shrinkExpr(cond), body.map(shrinkStmt))

/** Shrinks an expression by simplifying its structure and removing unnecessary
  * complexity by replacing the `and` and `or` operators with if expressions.
  *
  * @param e
  *   the expression to shrink
  * @return
  *   a simplified version of the expression
  */
def shrinkExpr(e: LWhile.Expr): LWhile.Expr = e match
  case LWhile.UnaryNumericOp(op, e) => LWhile.UnaryNumericOp(op, shrinkExpr(e))
  case LWhile.UnaryLogicOp(op, e)   => LWhile.UnaryLogicOp(op, shrinkExpr(e))
  case LWhile.BinaryNumericOp(op, lhs, rhs) =>
    LWhile.BinaryNumericOp(op, shrinkExpr(lhs), shrinkExpr(rhs))
  case LWhile.BinaryLogicOp(BinaryLogicOperator.And, e1, e2) =>
    LWhile.IfExpr(shrinkExpr(e1), shrinkExpr(e2), LWhile.ConstantBool(false))
  case LWhile.BinaryLogicOp(BinaryLogicOperator.Or, e1, e2) =>
    LWhile.IfExpr(shrinkExpr(e1), LWhile.ConstantBool(true), shrinkExpr(e2))
  case Expr.Compare(cmp, lhs, rhs) =>
    LWhile.Compare(cmp, shrinkExpr(lhs), shrinkExpr(rhs))
  case Expr.IfExpr(condExpr, thenExpr, elseExpr) =>
    LWhile.IfExpr(
      shrinkExpr(condExpr),
      shrinkExpr(thenExpr),
      shrinkExpr(elseExpr)
    )
  case _ => e
