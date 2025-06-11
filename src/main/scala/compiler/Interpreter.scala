package compiler

import CommonNodes.*
import scala.io.StdIn

/** Evaluates and partially evaluates LInt programs */
object LIntInterpreter {

  /** Fully evaluates an [[Expr]] to a [[Long]]
    *
    * @param e
    *   the expression to evaluate
    * @return
    *   the result of evaluating the expression
    */
  def evalExpr(e: LWhile.Expr): Long = e match
    case LWhile.Constant(n) => n
    case LWhile.UnaryNumericOp(op, e) =>
      op match
        case UnaryNumericOperator.USub => -evalExpr(e)
    case LWhile.BinaryNumericOp(op, lhs, rhs) =>
      op match
        case BinaryNumericOperator.Add => evalExpr(lhs) + evalExpr(rhs)
        case BinaryNumericOperator.Sub => evalExpr(lhs) - evalExpr(rhs)
    case LWhile.ReadIntCall => StdIn.readInt().toLong
    case other => sys error s"evaluation of expression $other not supported"

  /** Fully evaluates a statement
    *
    * @param stmt
    *   the statement to evaluate
    * @return
    *   the result of the evaluation (usually [[Unit]] or a [[Long]])
    */
  def evalStatement(stmt: LWhile.Stmt): Long | Unit = stmt match
    case LWhile.ExprStmt(e)  => evalExpr(e)
    case LWhile.PrintStmt(e) => println(evalExpr(e))
    case other => sys error s"evaluation of statement $other not supported"

  /** Fully evaluates a module
    *
    * @param module
    *   the module to evaluate
    * @return
    *   the result of the last statement or [[Unit]] if none
    */
  def evalModule(module: LWhile.Module): Long | Unit = module match
    case LWhile.Module(stmts) =>
      stmts.map(evalStatement).lastOption.getOrElse(())

  /** Partially evaluates an expression by constant folding
    *
    * @param e
    *   the expression to simplify
    * @return
    *   a simplified [[Expr]]
    */
  def partialEvalExpr(e: LWhile.Expr): LWhile.Expr = e match
    case c @ LWhile.Constant(_) => c
    case i @ LWhile.ReadIntCall => i
    case LWhile.UnaryNumericOp(op, e) =>
      val simplified = partialEvalExpr(e)
      op match
        case UnaryNumericOperator.USub =>
          simplified match
            case LWhile.Constant(n) => LWhile.Constant(-n)
            case _                  => LWhile.UnaryNumericOp(op, simplified)
    case LWhile.BinaryNumericOp(op, lhs, rhs) =>
      val left = partialEvalExpr(lhs)
      val right = partialEvalExpr(rhs)
      op match
        case BinaryNumericOperator.Add =>
          (left, right) match
            case (LWhile.Constant(a), LWhile.Constant(b)) =>
              LWhile.Constant(a + b)
            case _ => LWhile.BinaryNumericOp(op, left, right)
        case BinaryNumericOperator.Sub =>
          (left, right) match
            case (LWhile.Constant(a), LWhile.Constant(b)) =>
              LWhile.Constant(a - b)
            case _ => LWhile.BinaryNumericOp(op, left, right)
    case other => other

  /** Partially evaluates a statement by simplifying contained expressions
    *
    * @param stmt
    *   the statement to simplify
    * @return
    *   a simplified [[Stmt]]
    */
  def partialEvalStatement(stmt: LWhile.Stmt): LWhile.Stmt = stmt match
    case LWhile.ExprStmt(e)  => LWhile.ExprStmt(partialEvalExpr(e))
    case LWhile.PrintStmt(e) => LWhile.PrintStmt(partialEvalExpr(e))
    case other => sys error s"evaluation of statement $other not supported"

  /** Partially evaluates all statements in a module
    *
    * @param module
    *   the module to simplify
    * @return
    *   a simplified [[Module]]
    */
  def partialEvalModule(module: LWhile.Module): LWhile.Module = module match
    case LWhile.Module(stmts) =>
      LWhile.Module(stmts.map(partialEvalStatement))
}
