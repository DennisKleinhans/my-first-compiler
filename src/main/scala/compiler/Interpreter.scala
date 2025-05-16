package compiler

import CommonOperators.*
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
  def evalExpr(e: LVar.Expr): Long = e match
    case LVar.Constant(n) => n
    case LVar.UnaryOp(op, e) =>
      op match
        case UnaryOperator.USub => -evalExpr(e)
    case LVar.BinaryOp(op, lhs, rhs) =>
      op match
        case BinaryOperator.Add => evalExpr(lhs) + evalExpr(rhs)
        case BinaryOperator.Sub => evalExpr(lhs) - evalExpr(rhs)
    case LVar.Call(LVar.Identifier("input_int"), Nil) => StdIn.readInt().toLong
    case LVar.Call(f, _) =>
      sys error s"unknown function call: $f"
    case other => sys error s"evaluation of expression $other not supported"

  /** Fully evaluates a statement
    *
    * @param stmt
    *   the statement to evaluate
    * @return
    *   the result of the evaluation (usually [[Unit]] or a [[Long]])
    */
  def evalStatement(stmt: LVar.Stmt): Long | Unit = stmt match
    case LVar.ExprStmt(e)  => evalExpr(e)
    case LVar.PrintStmt(e) => println(evalExpr(e))
    case other => sys error s"evaluation of statement $other not supported"

  /** Fully evaluates a module
    *
    * @param module
    *   the module to evaluate
    * @return
    *   the result of the last statement or [[Unit]] if none
    */
  def evalModule(module: LVar.Module): Long | Unit = module match
    case LVar.Module(stmts) =>
      stmts.map(evalStatement).lastOption.getOrElse(())

  /** Partially evaluates an expression by constant folding
    *
    * @param e
    *   the expression to simplify
    * @return
    *   a simplified [[Expr]]
    */
  def partialEvalExpr(e: LVar.Expr): LVar.Expr = e match
    case c @ LVar.Constant(_)                             => c
    case i @ LVar.Call(LVar.Identifier("input_int"), Nil) => i
    case LVar.UnaryOp(op, e) =>
      val simplified = partialEvalExpr(e)
      op match
        case UnaryOperator.USub =>
          simplified match
            case LVar.Constant(n) => LVar.Constant(-n)
            case _                => LVar.UnaryOp(op, simplified)
    case LVar.BinaryOp(op, lhs, rhs) =>
      val left = partialEvalExpr(lhs)
      val right = partialEvalExpr(rhs)
      op match
        case BinaryOperator.Add =>
          (left, right) match
            case (LVar.Constant(a), LVar.Constant(b)) => LVar.Constant(a + b)
            case _ => LVar.BinaryOp(op, left, right)
        case BinaryOperator.Sub =>
          (left, right) match
            case (LVar.Constant(a), LVar.Constant(b)) => LVar.Constant(a - b)
            case _ => LVar.BinaryOp(op, left, right)
    case other => other

  /** Partially evaluates a statement by simplifying contained expressions
    *
    * @param stmt
    *   the statement to simplify
    * @return
    *   a simplified [[Stmt]]
    */
  def partialEvalStatement(stmt: LVar.Stmt): LVar.Stmt = stmt match
    case LVar.ExprStmt(e)  => LVar.ExprStmt(partialEvalExpr(e))
    case LVar.PrintStmt(e) => LVar.PrintStmt(partialEvalExpr(e))
    case other => sys error s"evaluation of statement $other not supported"

  /** Partially evaluates all statements in a module
    *
    * @param module
    *   the module to simplify
    * @return
    *   a simplified [[Module]]
    */
  def partialEvalModule(module: LVar.Module): LVar.Module = module match
    case LVar.Module(stmts) =>
      LVar.Module(stmts.map(partialEvalStatement))
}
