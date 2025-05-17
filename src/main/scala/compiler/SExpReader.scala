package compiler

import lang.SExp
import lang.SExp.*
import CommonNodes.*

/** A trait for converting S-Expressions into expressions [[Expr]], statements
  * [[Stmt]], and modules [[Module]]. This trait provides a default structure
  * for all implementations.
  */
trait Reader {

  /** Converts an S-Expression into a corresponding [[Expr]].
    *
    * @param sexp
    *   the S-Expression to convert
    * @return
    *   a corresponding [[Expr]]
    */
  def fromSExpToExpr(sexp: SExp): LVar.Expr

  /** Converts an S-Expression into a corresponding [[Stmt]].
    *
    * @param sexpr
    *   the S-Expression to convert
    * @return
    *   a corresponding [[Stmt]]
    */
  def fromSExpToStmt(sexp: SExp): LVar.Stmt

  /** Converts an S-Expression into a full [[Module]].
    *
    * Recognized form:
    *   - `(Module stmt1 stmt2 ...)` → a list of statements in module form
    *
    * @param sexpr
    *   the S-Expression to convert
    * @return
    *   a corresponding [[Module]]
    */
  def fromSExpToModule(sexp: SExp): LVar.Module
}

/** Parser for converting S-Expressions into [[AST]]s.
  *
  * Provides methods to translate serialized S-Expressions into structured LInt
  * language features.
  */
object LIntReader extends Reader {

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
  def fromSExpToExpr(sexpr: SExp): LVar.Expr = sexpr match
    case Node(Symbol("Constant") :: Number(n) :: Nil) =>
      LVar.Constant(n)
    case Node(
          Symbol("Call") :: Node(
            Symbol("Variable") :: Symbol(name) :: Nil
          ) :: argsNode :: Nil
        ) =>
      val args = argsNode match
        case Node(elements) => elements.map(fromSExpToExpr)
        case _              => sys.error("invalid argument list: " + argsNode)
      LVar.Call(Identifier(name), args)
    case Node(Symbol("Unary") :: Symbol("Neg") :: e :: Nil) =>
      LVar.UnaryOp(UnaryOperator.USub, fromSExpToExpr(e))
    case Node(Symbol("Binary") :: Symbol("Add") :: lhs :: rhs :: Nil) =>
      LVar.BinaryOp(
        BinaryOperator.Add,
        fromSExpToExpr(lhs),
        fromSExpToExpr(rhs)
      )
    case Node(Symbol("Binary") :: Symbol("Sub") :: lhs :: rhs :: Nil) =>
      LVar.BinaryOp(
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
  def fromSExpToStmt(sexpr: SExp): LVar.Stmt = sexpr match
    case Node(List(Node(Symbol("Expr") :: exprNode :: Nil))) =>
      fromSExpToExpr(exprNode) match
        case LVar.Call(Identifier("print"), List(arg)) =>
          LVar.PrintStmt(arg)
        case other => LVar.ExprStmt(other)
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
  def fromSExpToModule(sexpr: SExp): LVar.Module = sexpr match
    case Node(Symbol("Module") :: stmtSexps) =>
      val stmts = stmtSexps.map(fromSExpToStmt)
      LVar.Module(stmts)
    case other =>
      sys.error("invalid module: " + other)
}

/** Parser for converting S-Expressions into [[AST]]s.
  *
  * Provides methods to translate serialized S-Expressions into structured LVar
  * language features.
  */
object LVarReader extends Reader {

  /** Converts an S-Expression into a corresponding [[Expr]].
    *
    * Supported expression forms:
    *   - `(Constant n)` → numeric literal
    *   - `(Call (Variable name) args...)` → function call
    *   - `(Unary Neg e)` → unary negation
    *   - `(Binary Add lhs rhs)` → addition
    *   - `(Binary Sub lhs rhs)` → subtraction
    *   - `(Variable name)` → variable reference
    *
    * @param sexpr
    *   the S-Expression to convert
    * @return
    *   a corresponding [[Expr]]
    * @throws java.lang.RuntimeException
    *   if the expression is invalid
    */
  def fromSExpToExpr(sexp: SExp): LVar.Expr = sexp match
    case Node(Symbol("Variable") :: Symbol(name) :: Nil) =>
      LVar.Variable(Identifier(name))
    case other => LIntReader.fromSExpToExpr(other)

    /** Converts an S-Expression into a corresponding [[Stmt]].
      *
      * Recognized forms:
      *   - `(Expr expr)` → an expression statement or a print statement (if the
      *     expression is a call to `print`, it's converted into a print
      *     statement)
      *   - `(Assign name expr)` → an assignment statement
      *
      * @param sexpr
      *   the S-Expression to convert
      * @return
      *   a corresponding [[Stmt]]
      * @throws java.lang.RuntimeException
      *   if the statement is invalid
      */
  def fromSExpToStmt(sexp: SExp): LVar.Stmt = sexp match
    case Node(
          List(Node(Symbol("Assign") :: Symbol(name) :: assignExpr :: Nil))
        ) =>
      LVar.AssignStmt(Identifier(name), fromSExpToExpr(assignExpr))
    case other => LIntReader.fromSExpToStmt(other)

  def fromSExpToModule(sexp: SExp): LVar.Module = sexp match
    case Node(Symbol("Module") :: stmtSexps) =>
      val stmts = stmtSexps.map(fromSExpToStmt)
      LVar.Module(stmts)
    case other =>
      sys.error("invalid module: " + other)
}
