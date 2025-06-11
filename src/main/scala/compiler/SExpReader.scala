package compiler

import lang.SExp
import lang.SExp.*
import CommonNodes.*

/** Parser for converting S-Expressions into `AST`s.
  *
  * Provides methods to translate serialized S-Expressions into structured LVar
  * language features.
  */
object LIfReader {

  /** Converts an S-Expression into a corresponding `CompareOperator`.
    *
    * @param sexp
    *   the `SExp` to convert
    * @return
    *   a corresponding `CompareOperator`
    * @throws java.lang.RuntimeException
    *   if the S-Expression does not represent a valid compare operator
    */
  def fromSExpToCompareOperator(sexp: SExp): CompareOperator = sexp match
    case Symbol("Eq")  => CompareOperator.Eq
    case Symbol("Neq") => CompareOperator.NotEq
    case Symbol("Lt")  => CompareOperator.Lt
    case Symbol("Le")  => CompareOperator.LtE
    case Symbol("Gt")  => CompareOperator.Gt
    case Symbol("Ge")  => CompareOperator.GtE
    case _             => sys error "Not a supported compare operator"

  /** Converts an S-Expression into a corresponding `Expr`.
    *
    * @param sexpr
    *   the `SExp` to convert
    * @return
    *   a corresponding `Expr`
    * @throws java.lang.RuntimeException
    *   if the expression is invalid
    */
  def fromSExpToExpr(sexp: SExp): LWhile.Expr = sexp match
    case Node(Symbol("Variable") :: Symbol(name) :: Nil) =>
      LWhile.Variable(Identifier(name))

    case Node(Symbol("Constant") :: Number(n) :: Nil) =>
      LWhile.Constant(n)

    case Node(Symbol("True") :: Nil) => LWhile.ConstantBool(true)

    case Node(Symbol("False") :: Nil) => LWhile.ConstantBool(false)

    case Node(
          Symbol("Call") ::
          Node(Symbol("Variable") :: Symbol("read_int") :: Nil) ::
          Node(Nil) ::
          Nil
        ) =>
      LWhile.ReadIntCall

    case Node(
          Symbol("Call") :: Node(
            Symbol("Variable") :: Symbol(name) :: Nil
          ) :: _ :: Nil
        ) if (name != "print") =>
      sys.error(s"Unknown function call: $name")

    case Node(Symbol("Unary") :: Symbol("Neg") :: e :: Nil) =>
      LWhile.UnaryNumericOp(UnaryNumericOperator.USub, fromSExpToExpr(e))

    case Node(Symbol("Binary") :: Symbol("Add") :: lhs :: rhs :: Nil) =>
      LWhile.BinaryNumericOp(
        BinaryNumericOperator.Add,
        fromSExpToExpr(lhs),
        fromSExpToExpr(rhs)
      )

    case Node(Symbol("Binary") :: Symbol("Sub") :: lhs :: rhs :: Nil) =>
      LWhile.BinaryNumericOp(
        BinaryNumericOperator.Sub,
        fromSExpToExpr(lhs),
        fromSExpToExpr(rhs)
      )

    case Node(Symbol("Binary") :: Symbol("And") :: e1 :: e2 :: Nil) =>
      LWhile.BinaryLogicOp(
        BinaryLogicOperator.And,
        fromSExpToExpr(e1),
        fromSExpToExpr(e2)
      )

    case Node(Symbol("Binary") :: Symbol("Or") :: e1 :: e2 :: Nil) =>
      LWhile.BinaryLogicOp(
        BinaryLogicOperator.And,
        fromSExpToExpr(e1),
        fromSExpToExpr(e2)
      )

    case Node(Symbol("Unary") :: Symbol("Not") :: e :: Nil) =>
      LWhile.UnaryLogicOp(UnaryLogicOperator.Not, fromSExpToExpr(e))

    // if expression
    case Node(Symbol("IfExp") :: test :: thenExpr :: elseExpr :: Nil) =>
      LWhile.IfExpr(
        fromSExpToExpr(test),
        fromSExpToExpr(thenExpr),
        fromSExpToExpr(elseExpr)
      )

    // compare node
    case Node((Symbol("Binary") :: cmp :: e1 :: e2 :: Nil)) =>
      LWhile.Compare(
        fromSExpToCompareOperator(cmp),
        fromSExpToExpr(e1),
        fromSExpToExpr(e2)
      )

    case other =>
      sys.error(s"invalid expression: $other")

  /** Converts an S-Expression into a corresponding `Stmt`.
    *
    * @param sexpr
    *   the `SExp` to convert
    * @return
    *   a corresponding `Stmt`
    * @throws java.lang.RuntimeException
    *   if the statement is invalid
    */
  def fromSExpToStmt(sexp: SExp): LWhile.Stmt = sexp match
    case Node(Symbol("Assign") :: Symbol(name) :: assignExpr :: Nil) =>
      LWhile.AssignStmt(Identifier(name), fromSExpToExpr(assignExpr))

    case Node(
          Symbol("Expr") :: Node(
            Symbol("Call") ::
            Node(Symbol("Variable") :: Symbol("print") :: Nil) ::
            Node(List(arg)) :: Nil
          ) :: Nil
        ) =>
      LWhile.PrintStmt(fromSExpToExpr(arg))

    case Node(Symbol("If") :: test :: Node(thn) :: Node(els) :: Nil) =>
      LWhile.IfStmt(
        fromSExpToExpr(test),
        thn.map(fromSExpToStmt),
        els.map(fromSExpToStmt)
      )

    case Node(Symbol("Expr") :: exprNode :: Nil) =>
      fromSExpToExpr(exprNode) match
        case other =>
          LWhile.ExprStmt(other)

    case Node(Symbol("While") :: test :: Node(body) :: Nil) =>
      LWhile.WhileStmt(fromSExpToExpr(test), body.map(fromSExpToStmt))

    case other =>
      sys.error(s"invalid statement: $other")

  /** Converts an S-Expression into a corresponding `Module`.
    *
    * @param sexp
    *   the `SExp` to convert
    * @return
    *   a corresponding `Module`
    * @throws java.lang.RuntimeException
    *   if the S-Expression does not represent a valid `Module`
    */
  def fromSExpToModule(sexp: SExp): LWhile.Module = sexp match
    case Node(Symbol("Module") :: rest) =>
      // rest may be a direct list of stmts or a single wrapper node
      val stmtNodes = rest match
        case List(Node(inner)) => inner
        case many              => many
      LWhile.Module(stmtNodes.map(fromSExpToStmt))

    case other =>
      sys.error(s"invalid module: $other")
}
