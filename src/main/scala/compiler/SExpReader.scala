package compiler

import lang.SExp
import lang.SExp.*
import CommonNodes.*

/** Parser for converting S-Expressions into [[AST]]s.
  *
  * Provides methods to translate serialized S-Expressions into structured LVar
  * language features.
  */
object LVarReader {

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
  def fromSExpToExpr(sexp: SExp): LIf.Expr = sexp match
    case Node(Symbol("Variable") :: Symbol(name) :: Nil) =>
      LIf.Variable(Identifier(name))

    case Node(Symbol("Constant") :: Number(n) :: Nil) =>
      LIf.Constant(n)

    case Node(
          Symbol("Call") ::
          Node(Symbol("Variable") :: Symbol("read_int") :: Nil) ::
          Node(Nil) ::
          Nil
        ) =>
      LIf.ReadIntCall

    case Node(
          Symbol("Call") :: Node(
            Symbol("Variable") :: Symbol(name) :: Nil
          ) :: _ :: Nil
        ) if (name != "print") =>
      sys.error(s"Unknown function call: $name")

    case Node(Symbol("Unary") :: Symbol("Neg") :: e :: Nil) =>
      LIf.UnaryNumericOp(UnaryNumericOperator.USub, fromSExpToExpr(e))

    case Node(Symbol("Binary") :: Symbol("Add") :: lhs :: rhs :: Nil) =>
      LIf.BinaryNumericOp(
        BinaryNumericOperator.Add,
        fromSExpToExpr(lhs),
        fromSExpToExpr(rhs)
      )

    case Node(Symbol("Binary") :: Symbol("Sub") :: lhs :: rhs :: Nil) =>
      LIf.BinaryNumericOp(
        BinaryNumericOperator.Sub,
        fromSExpToExpr(lhs),
        fromSExpToExpr(rhs)
      )

    case other =>
      sys.error(s"invalid expression: $other")

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
  def fromSExpToStmt(sexp: SExp): LIf.Stmt = sexp match
    case Node(Symbol("Assign") :: Symbol(name) :: assignExpr :: Nil) =>
      LIf.AssignStmt(Identifier(name), fromSExpToExpr(assignExpr))

    case Node(
          Symbol("Expr") :: Node(
            Symbol("Call") ::
            Node(Symbol("Variable") :: Symbol("print") :: Nil) ::
            Node(List(arg)) :: Nil
          ) :: Nil
        ) =>
      LIf.PrintStmt(fromSExpToExpr(arg))

    case Node(Symbol("Expr") :: exprNode :: Nil) =>
      fromSExpToExpr(exprNode) match
        case other =>
          LIf.ExprStmt(other)

    case other =>
      sys.error(s"invalid statement: $other")

  /** Converts an S-Expression into a corresponding [[Module]].
    *
    * @param sexp
    * @return
    *   a corresponding [[LVar.Module]]
    */
  def fromSExpToModule(sexp: SExp): LIf.Module = sexp match
    case Node(Symbol("Module") :: rest) =>
      // rest may be a direct list of stmts or a single wrapper node
      val stmtNodes = rest match
        case List(Node(inner)) => inner
        case many              => many
      LIf.Module(stmtNodes.map(fromSExpToStmt))

    case other =>
      sys.error(s"invalid module: $other")
}
