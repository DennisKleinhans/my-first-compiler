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
  def evalExpr(e: LIf.Expr): Long = e match
    case LIf.Constant(n) => n
    case LIf.UnaryNumericOp(op, e) =>
      op match
        case UnaryNumericOperator.USub => -evalExpr(e)
    case LIf.BinaryNumericOp(op, lhs, rhs) =>
      op match
        case BinaryNumericOperator.Add => evalExpr(lhs) + evalExpr(rhs)
        case BinaryNumericOperator.Sub => evalExpr(lhs) - evalExpr(rhs)
    case LIf.ReadIntCall => StdIn.readInt().toLong
    case other => sys error s"evaluation of expression $other not supported"

  /** Fully evaluates a statement
    *
    * @param stmt
    *   the statement to evaluate
    * @return
    *   the result of the evaluation (usually [[Unit]] or a [[Long]])
    */
  def evalStatement(stmt: LIf.Stmt): Long | Unit = stmt match
    case LIf.ExprStmt(e)  => evalExpr(e)
    case LIf.PrintStmt(e) => println(evalExpr(e))
    case other => sys error s"evaluation of statement $other not supported"

  /** Fully evaluates a module
    *
    * @param module
    *   the module to evaluate
    * @return
    *   the result of the last statement or [[Unit]] if none
    */
  def evalModule(module: LIf.Module): Long | Unit = module match
    case LIf.Module(stmts) =>
      stmts.map(evalStatement).lastOption.getOrElse(())

  /** Partially evaluates an expression by constant folding
    *
    * @param e
    *   the expression to simplify
    * @return
    *   a simplified [[Expr]]
    */
  def partialEvalExpr(e: LIf.Expr): LIf.Expr = e match
    case c @ LIf.Constant(_)  => c
    case i @ LIf.ReadIntCall => i
    case LIf.UnaryNumericOp(op, e) =>
      val simplified = partialEvalExpr(e)
      op match
        case UnaryNumericOperator.USub =>
          simplified match
            case LIf.Constant(n) => LIf.Constant(-n)
            case _               => LIf.UnaryNumericOp(op, simplified)
    case LIf.BinaryNumericOp(op, lhs, rhs) =>
      val left = partialEvalExpr(lhs)
      val right = partialEvalExpr(rhs)
      op match
        case BinaryNumericOperator.Add =>
          (left, right) match
            case (LIf.Constant(a), LIf.Constant(b)) => LIf.Constant(a + b)
            case _ => LIf.BinaryNumericOp(op, left, right)
        case BinaryNumericOperator.Sub =>
          (left, right) match
            case (LIf.Constant(a), LIf.Constant(b)) => LIf.Constant(a - b)
            case _ => LIf.BinaryNumericOp(op, left, right)
    case other => other

  /** Partially evaluates a statement by simplifying contained expressions
    *
    * @param stmt
    *   the statement to simplify
    * @return
    *   a simplified [[Stmt]]
    */
  def partialEvalStatement(stmt: LIf.Stmt): LIf.Stmt = stmt match
    case LIf.ExprStmt(e)  => LIf.ExprStmt(partialEvalExpr(e))
    case LIf.PrintStmt(e) => LIf.PrintStmt(partialEvalExpr(e))
    case other => sys error s"evaluation of statement $other not supported"

  /** Partially evaluates all statements in a module
    *
    * @param module
    *   the module to simplify
    * @return
    *   a simplified [[Module]]
    */
  def partialEvalModule(module: LIf.Module): LIf.Module = module match
    case LIf.Module(stmts) =>
      LIf.Module(stmts.map(partialEvalStatement))
}
