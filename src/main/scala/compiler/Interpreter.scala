package compiler

import compiler.AST.Expr.*
import compiler.AST.Stmt.*
import scala.io.StdIn
import compiler.AST.*

/** Evaluates and partially evaluates LInt programs */
object LIntInterpreter {

  /** Fully evaluates an [[Expr]] to a [[Long]]
    *
    * @param e
    *   the expression to evaluate
    * @return
    *   the result of evaluating the expression
    */
  def evalExpr(e: Expr): Long = e match
    case Constant(n) => n
    case UnaryOp(op, e) =>
      op match
        case UnaryOperator.USub => -evalExpr(e)
    case BinaryOp(op, lhs, rhs) =>
      op match
        case BinaryOperator.Add => evalExpr(lhs) + evalExpr(rhs)
        case BinaryOperator.Sub => evalExpr(lhs) - evalExpr(rhs)
    case Call("input_int", Nil) => StdIn.readInt().toLong
    case Call(f, _) =>
      sys error s"unknown function call: $f"
    case other => sys error s"evaluation of expression $other not supported"

  /** Fully evaluates a statement
    *
    * @param stmt
    *   the statement to evaluate
    * @return
    *   the result of the evaluation (usually [[Unit]] or a [[Long]])
    */
  def evalStatement(stmt: Stmt): Long | Unit = stmt match
    case ExprStmt(e)  => evalExpr(e)
    case PrintStmt(e) => println(evalExpr(e))
    case other => sys error s"evaluation of statement $other not supported"

  /** Fully evaluates a module
    *
    * @param module
    *   the module to evaluate
    * @return
    *   the result of the last statement or [[Unit]] if none
    */
  def evalModule(module: Module): Long | Unit = module match
    case Module(stmts) =>
      stmts.map(evalStatement).lastOption.getOrElse(())

  /** Partially evaluates an expression by constant folding
    *
    * @param e
    *   the expression to simplify
    * @return
    *   a simplified [[Expr]]
    */
  def partialEvalExpr(e: Expr): Expr = e match
    case c @ Constant(_)            => c
    case i @ Call("input_int", Nil) => i
    case UnaryOp(op, e) =>
      val simplified = partialEvalExpr(e)
      op match
        case UnaryOperator.USub =>
          simplified match
            case Constant(n) => Constant(-n)
            case _           => UnaryOp(op, simplified)
    case BinaryOp(op, lhs, rhs) =>
      val left = partialEvalExpr(lhs)
      val right = partialEvalExpr(rhs)
      op match
        case BinaryOperator.Add =>
          (left, right) match
            case (Constant(a), Constant(b)) => Constant(a + b)
            case _                          => BinaryOp(op, left, right)
        case BinaryOperator.Sub =>
          (left, right) match
            case (Constant(a), Constant(b)) => Constant(a - b)
            case _                          => BinaryOp(op, left, right)
    case other => other

  /** Partially evaluates a statement by simplifying contained expressions
    *
    * @param stmt
    *   the statement to simplify
    * @return
    *   a simplified [[Stmt]]
    */
  def partialEvalStatement(stmt: Stmt): Stmt = stmt match
    case ExprStmt(e)  => ExprStmt(partialEvalExpr(e))
    case PrintStmt(e) => PrintStmt(partialEvalExpr(e))
    case other => sys error s"evaluation of statement $other not supported"

  /** Partially evaluates all statements in a module
    *
    * @param module
    *   the module to simplify
    * @return
    *   a simplified [[Module]]
    */
  def partialEvalModule(module: Module): Module = module match
    case Module(stmts) =>
      Module(stmts.map(partialEvalStatement))
}
