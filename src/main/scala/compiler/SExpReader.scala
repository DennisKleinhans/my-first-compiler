package compiler

import lang.SExp
import lang.SExp.*
import compiler.AST
import compiler.AST.*
import compiler.AST.Expr.*
import compiler.AST.Stmt.*

/** Parser for converting S-Expressions into [[AST]]s.
  *
  * Provides methods to translate serialized S-Expressions into structured LInt
  * language features.
  */
object LIntReader {

  /** Converts an S-Expression into a corresponding [[Expr]].
    *
    * Supported expression forms:
    *   - `(Constant n)` → numeric literal
    *   - `(Call (Variable name) args...)` → function call
    *   - `(Unary Neg e)` → unary negation
    *   - `(Binary Add lhs rhs)` → addition
    *   - `(Binary Sub lhs rhs)` → subtraction
    *
    * @param sexpr
    *   the S-Expression to convert
    * @return
    *   a corresponding [[Expr]]
    * @throws java.lang.RuntimeException
    *   if the expression is invalid
    */
  def fromSExpToExpr(sexpr: SExp): Expr = sexpr match
    case Node(List(Symbol("Constant"), Number(n))) =>
      Constant(n)
    case Node(
          Symbol("Call") :: Node(
            Symbol("Variable") :: Symbol(name) :: Nil
          ) :: argsNode :: Nil
        ) =>
      val args = argsNode match
        case Node(elements) => elements.map(fromSExpToExpr)
        case _              => sys.error("invalid argument list: " + argsNode)
      Call(name, args)
    case Node(Symbol("Unary") :: Symbol("Neg") :: e :: Nil) =>
      UnaryOp(UnaryOperator.USub, fromSExpToExpr(e))
    case Node(Symbol("Binary") :: Symbol("Add") :: lhs :: rhs :: Nil) =>
      BinaryOp(
        BinaryOperator.Add,
        fromSExpToExpr(lhs),
        fromSExpToExpr(rhs)
      )
    case Node(Symbol("Binary") :: Symbol("Sub") :: lhs :: rhs :: Nil) =>
      BinaryOp(
        BinaryOperator.Sub,
        fromSExpToExpr(lhs),
        fromSExpToExpr(rhs)
      )
    case other =>
      sys.error("invalid expression: " + other)

  /** Converts an S-Expression into a corresponding [[Stmt]].
    *
    * Recognized forms:
    *   - `(Expr expr)` → an expression statement or a print statement (if the
    *     expression is a call to `print`, it's converted into a print
    *     statement)
    *
    * @param sexpr
    *   the S-Expression to convert
    * @return
    *   a corresponding [[Stmt]]
    * @throws java.lang.RuntimeException
    *   if the statement is invalid
    */
  def fromSExpToStmt(sexpr: SExp): Stmt = sexpr match
    case Node(List(Node(List(Symbol("Expr"), exprNode)))) =>
      fromSExpToExpr(exprNode) match
        case Call("print", List(arg)) => PrintStmt(arg)
        case other                    => ExprStmt(other)
    case other =>
      sys.error("invalid statement: " + other)

  /** Converts an S-Expression into a full [[Module]].
    *
    * Recognized form:
    *   - `(Module stmt1 stmt2 ...)` → a list of statements in module form
    *
    * @param sexpr
    *   the S-Expression to convert
    * @return
    *   a corresponding [[Module]]
    * @throws java.lang.RuntimeException
    *   if the module structure is invalid
    */
  def fromSExpToModule(sexpr: SExp): Module = sexpr match
    case Node(Symbol("Module") :: stmtSexps) =>
      val stmts = stmtSexps.map(fromSExpToStmt)
      Module(stmts)
    case other =>
      sys.error("invalid module: " + other)
}
