package compiler

import compiler.LIf.Expr
import CommonNodes.*
import compiler.LIf.Stmt

/**
  * Shrinks the LIf module by simplifying its statements and expressions.
  *
  * @param module
  * @return
  */
def shrinkModule(module: LIf.Module): LIf.Module =
  LIf.Module(module.stmts.map(shrinkStmt))

/**
  * Shrinks a single statement by simplifying its expressions.
  *
  * @param stmt
  * @return
  */
def shrinkStmt(stmt: LIf.Stmt): LIf.Stmt = stmt match
  case LIf.ExprStmt(e)       => LIf.ExprStmt(shrinkExpr(e))
  case LIf.PrintStmt(e)      => LIf.PrintStmt(shrinkExpr(e))
  case LIf.AssignStmt(id, e) => LIf.AssignStmt(id, shrinkExpr(e))
  case LIf.IfStmt(cond, thenBranch, elseBranch) =>
    LIf.IfStmt(
      shrinkExpr(cond),
      thenBranch.map(shrinkStmt),
      elseBranch.map(shrinkStmt)
    )

/**
  * Shrinks an expression by simplifying its structure and removing unnecessary
  * complexity by replacing the `and` and `or` operators with if expressions.
  *
  * @param e
  *   the expression to shrink
  * @return
  *   a simplified version of the expression
  */
def shrinkExpr(e: LIf.Expr): LIf.Expr = e match
  case LIf.UnaryNumericOp(op, e) => LIf.UnaryNumericOp(op, shrinkExpr(e))
  case LIf.UnaryLogicOp(op, e)   => LIf.UnaryLogicOp(op, shrinkExpr(e))
  case LIf.BinaryNumericOp(op, lhs, rhs) =>
    LIf.BinaryNumericOp(op, shrinkExpr(lhs), shrinkExpr(rhs))
  case LIf.BinaryLogicOp(BinaryLogicOperator.And, e1, e2) =>
    LIf.IfExpr(shrinkExpr(e1), shrinkExpr(e2), LIf.ConstantBool(false))
  case LIf.BinaryLogicOp(BinaryLogicOperator.Or, e1, e2) =>
    LIf.IfExpr(shrinkExpr(e1), LIf.ConstantBool(true), shrinkExpr(e2))
  case Expr.Compare(cmp, lhs, rhs) =>
    LIf.Compare(cmp, shrinkExpr(lhs), shrinkExpr(rhs))
  case Expr.IfExpr(condExpr, thenExpr, elseExpr) =>
    LIf.IfExpr(shrinkExpr(condExpr), shrinkExpr(thenExpr), shrinkExpr(elseExpr))
  case _ => e
