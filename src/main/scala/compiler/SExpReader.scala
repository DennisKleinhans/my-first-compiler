package compiler

import lang.SExp
import lang.SExp.*
import CommonNodes.*

/** Parser for converting S-Expressions into `AST`s.
  *
  * Provides methods to translate serialized S-Expressions into structured LVar
  * language features.
  */
object LCoreReader {

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
    case Symbol("Is")  => CompareOperator.Is
    case other            => sys error f" $other is not a supported compare operator"

  /** Converts an S-Expression into a corresponding `Expr`.
    *
    * @param sexpr
    *   the `SExp` to convert
    * @return
    *   a corresponding `Expr`
    * @throws java.lang.RuntimeException
    *   if the expression is invalid
    */
  def fromSExpToExpr(sexp: SExp): LCore.Expr = sexp match
    case Node(Symbol("Variable") :: Symbol(name) :: Nil) =>
      LCore.Variable(Identifier(name))

    case Node(Symbol("Constant") :: Number(n) :: Nil) =>
      LCore.Constant(n)

    case Node(Symbol("True") :: Nil) => LCore.ConstantBool(true)

    case Node(Symbol("False") :: Nil) => LCore.ConstantBool(false)

    case Node(
          Symbol("Call") ::
          Node(Symbol("Variable") :: Symbol("read_int") :: Nil) ::
          Node(Nil) ::
          Nil
        ) =>
      LCore.ReadIntCall

    case Node(Symbol("Unary") :: Symbol("Neg") :: e :: Nil) =>
      LCore.UnaryNumericOp(UnaryNumericOperator.USub, fromSExpToExpr(e))

    case Node(Symbol("Binary") :: Symbol("Add") :: lhs :: rhs :: Nil) =>
      LCore.BinaryNumericOp(
        BinaryNumericOperator.Add,
        fromSExpToExpr(lhs),
        fromSExpToExpr(rhs)
      )

    case Node(Symbol("Binary") :: Symbol("Sub") :: lhs :: rhs :: Nil) =>
      LCore.BinaryNumericOp(
        BinaryNumericOperator.Sub,
        fromSExpToExpr(lhs),
        fromSExpToExpr(rhs)
      )

    case Node(Symbol("Binary") :: Symbol("And") :: e1 :: e2 :: Nil) =>
      LCore.BinaryLogicOp(
        BinaryLogicOperator.And,
        fromSExpToExpr(e1),
        fromSExpToExpr(e2)
      )

    case Node(Symbol("Binary") :: Symbol("Or") :: e1 :: e2 :: Nil) =>
      LCore.BinaryLogicOp(
        BinaryLogicOperator.And,
        fromSExpToExpr(e1),
        fromSExpToExpr(e2)
      )

    case Node(Symbol("Unary") :: Symbol("Not") :: e :: Nil) =>
      LCore.UnaryLogicOp(UnaryLogicOperator.Not, fromSExpToExpr(e))

    // if expression
    case Node(Symbol("IfExp") :: test :: thenExpr :: elseExpr :: Nil) =>
      LCore.IfExpr(
        fromSExpToExpr(test),
        fromSExpToExpr(thenExpr),
        fromSExpToExpr(elseExpr)
      )

    // compare node
    case Node((Symbol("Binary") :: cmp :: e1 :: e2 :: Nil)) =>
      LCore.Compare(
        fromSExpToCompareOperator(cmp),
        fromSExpToExpr(e1),
        fromSExpToExpr(e2)
      )

    // Tuple
    case Node(Symbol("Tuple") :: Node(elements) :: Nil) =>
      LCore.Tuple(elements.map(fromSExpToExpr))

    // Tuple Projection
    case Node(Symbol("Subscript") :: tupleVar :: Number(index) :: Nil) =>
      LCore.TupleProjection(fromSExpToExpr(tupleVar), index)

    // Tuple len()
    case Node(
          Symbol("Call") :: Node(
            Symbol("Variable") :: Symbol("len") :: Nil
          ) :: Node(tupleVar :: Nil) :: Nil
        ) =>
      LCore.TupleLen(fromSExpToExpr(tupleVar))

    case Node(Symbol("Call") :: Node(Symbol("Variable") :: Symbol(name) :: Nil) :: Node(args) :: Nil) =>
      LCore.Call(name, args.map(fromSExpToExpr))

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
  def fromSExpToStmt(sexp: SExp): LCore.Stmt = sexp match
    case Node(Symbol("Assign") :: Symbol(name) :: assignExpr :: Nil) =>
      LCore.AssignStmt(Identifier(name), fromSExpToExpr(assignExpr))

    case Node(
          Symbol("Expr") :: Node(
            Symbol("Call") ::
            Node(Symbol("Variable") :: Symbol("print") :: Nil) ::
            Node(List(arg)) :: Nil
          ) :: Nil
        ) =>
      LCore.PrintStmt(fromSExpToExpr(arg))

    case Node(Symbol("If") :: test :: Node(thn) :: Node(els) :: Nil) =>
      LCore.IfStmt(
        fromSExpToExpr(test),
        thn.map(fromSExpToStmt),
        els.map(fromSExpToStmt)
      )

    case Node(Symbol("Expr") :: exprNode :: Nil) =>
      fromSExpToExpr(exprNode) match
        case other =>
          LCore.ExprStmt(other)

    case Node(Symbol("While") :: test :: Node(body) :: Nil) =>
      LCore.WhileStmt(fromSExpToExpr(test), body.map(fromSExpToStmt))

    case Node(Symbol("Return") :: (exprNode :: Nil)) =>
      LCore.ReturnStmt(fromSExpToExpr(exprNode))

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
  def fromSExpToModule(sexp: SExp): LCore.Module = sexp match
    case Node(Symbol("Module") :: Node(funs) :: Node(stmtsNode) :: Nil) =>
      val funDefs = funs.map(fromSExpToFunDef)
      val stmts = stmtsNode.map(fromSExpToStmt)
      LCore.Module(funDefs, stmts)

    case other =>
      sys.error(s"invalid module: $other")

  /** Converts an S-Expression into a corresponding `FunctionDef`.
    *
    * @param sexp
    *   the `SExp` to convert
    * @return
    *   a corresponding `FunctionDef`
    * @throws java.lang.RuntimeException
    *   if the S-Expression does not represent a valid `FunctionDef`
    */
  def fromSExpToFunDef(sexp: SExp): LCore.FunctionDef = sexp match
    case Node(
          Symbol("Fun") :: Symbol(name) :: Node(paramsSexp) :: Node(
            returnType
          ) :: Node(body) :: Nil
        ) =>
      val params = paramsSexp.map(fromSExpToParam)
      val bodyStmts = body.map(fromSExpToStmt)
      LCore.FunctionDef(name, params, bodyStmts)
    case other => sys error "got in FunDef: " + other

  /** Converts an S-Expression into a corresponding `Param`.
    *
    * @param sexp
    *   the `SExp` to convert
    * @return
    *   a corresponding `Param`
    * @throws java.lang.RuntimeException
    *   if the S-Expression does not represent a valid `Param`
    */
  def fromSExpToParam(sexp: SExp): Param = sexp match
    case Node(Symbol("Param") :: Symbol(name) :: _) =>
      Param(name)

    case other => sys error "got in Param: " + other

}
