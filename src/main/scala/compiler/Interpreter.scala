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
  def evalExpr(e: LCore.Expr): Long = e match
    case LCore.Constant(n) => n
    case LCore.UnaryNumericOp(op, e) =>
      op match
        case UnaryNumericOperator.USub => -evalExpr(e)
    case LCore.BinaryNumericOp(op, lhs, rhs) =>
      op match
        case BinaryNumericOperator.Add => evalExpr(lhs) + evalExpr(rhs)
        case BinaryNumericOperator.Sub => evalExpr(lhs) - evalExpr(rhs)
    case LCore.ReadIntCall => StdIn.readInt().toLong
    case other => sys error s"evaluation of expression $other not supported"

  /** Fully evaluates a statement
    *
    * @param stmt
    *   the statement to evaluate
    * @return
    *   the result of the evaluation (usually [[Unit]] or a [[Long]])
    */
  def evalStatement(stmt: LCore.Stmt): Long | Unit = stmt match
    case LCore.ExprStmt(e)  => evalExpr(e)
    case LCore.PrintStmt(e) => println(evalExpr(e))
    case other => sys error s"evaluation of statement $other not supported"

  /** Fully evaluates a module
    *
    * @param module
    *   the module to evaluate
    * @return
    *   the result of the last statement or [[Unit]] if none
    */
  def evalModule(module: LCore.Module): Long | Unit = module match
    case LCore.Module(stmts) =>
      stmts.map(evalStatement).lastOption.getOrElse(())

  /** Partially evaluates an expression by constant folding
    *
    * @param e
    *   the expression to simplify
    * @return
    *   a simplified [[Expr]]
    */
  def partialEvalExpr(e: LCore.Expr): LCore.Expr = e match
    case c @ LCore.Constant(_) => c
    case i @ LCore.ReadIntCall => i
    case LCore.UnaryNumericOp(op, e) =>
      val simplified = partialEvalExpr(e)
      op match
        case UnaryNumericOperator.USub =>
          simplified match
            case LCore.Constant(n) => LCore.Constant(-n)
            case _                 => LCore.UnaryNumericOp(op, simplified)
    case LCore.BinaryNumericOp(op, lhs, rhs) =>
      val left = partialEvalExpr(lhs)
      val right = partialEvalExpr(rhs)
      op match
        case BinaryNumericOperator.Add =>
          (left, right) match
            case (LCore.Constant(a), LCore.Constant(b)) =>
              LCore.Constant(a + b)
            case _ => LCore.BinaryNumericOp(op, left, right)
        case BinaryNumericOperator.Sub =>
          (left, right) match
            case (LCore.Constant(a), LCore.Constant(b)) =>
              LCore.Constant(a - b)
            case _ => LCore.BinaryNumericOp(op, left, right)
    case other => other

  /** Partially evaluates a statement by simplifying contained expressions
    *
    * @param stmt
    *   the statement to simplify
    * @return
    *   a simplified [[Stmt]]
    */
  def partialEvalStatement(stmt: LCore.Stmt): LCore.Stmt = stmt match
    case LCore.ExprStmt(e)  => LCore.ExprStmt(partialEvalExpr(e))
    case LCore.PrintStmt(e) => LCore.PrintStmt(partialEvalExpr(e))
    case other => sys error s"evaluation of statement $other not supported"

  /** Partially evaluates all statements in a module
    *
    * @param module
    *   the module to simplify
    * @return
    *   a simplified [[Module]]
    */
  def partialEvalModule(module: LCore.Module): LCore.Module = module match
    case LCore.Module(stmts) =>
      LCore.Module(stmts.map(partialEvalStatement))
}
