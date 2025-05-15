package compiler

import compiler.LVar.UnaryOperator
import compiler.LVar.BinaryOperator

/** The abstract syntax tree (AST) for the LVar language.
  *
  * Represents the structure of LVar programs after parsing and before
  * interpretation or compilation.
  */
object LVar {

  /** A module is the top-level program structure in LVar.
    *
    * @param stmts
    *   the list of statements contained in the module
    */
  case class Module(stmts: List[Stmt])

  /** A statement in LVar represents either a standalone expression, a print
    * operation or an assign operation.
    */
  enum Stmt:
    case ExprStmt(e: Expr)
    case PrintStmt(e: Expr)
    case AssignStmt(name: String, e: Expr)

  /** An expression in LVar can be a constant value, a unary operation, a binary
    * operation, or a function call.
    */
  enum Expr:
    case Constant(n: Long)
    case UnaryOp(op: UnaryOperator, e: Expr)
    case BinaryOp(op: BinaryOperator, left: Expr, right: Expr)
    case Call(name: String, args: List[Expr])
    case Variable(name: String)

  /** A unary operator represents an operation with a single operand. */
  enum UnaryOperator:
    case USub

  /** A binary operator represents an operation with two operands. */
  enum BinaryOperator:
    case Add
    case Sub
}

/** The abstract syntax tree (AST) for the LMonVar language.
  *
  * Represents the structure of LMonVar programs after parsing and before
  * interpretation or compilation.
  */
object LMonVar {

  /** A module is the top-level program structure in LMonVar.
    *
    * @param stmts
    *   the list of statements contained in the module
    */
  case class Module(stmts: List[Stmt])

  /** A statement in LMonVar represents either a standalone expression, a print
    * operation or an assign operation.
    */
  enum Stmt:
    case AssignStmt(name: String, rhs: Expr)
    case PrintStmt(a: Atom)
    case ExprStmt(a: Atom)

  /** An atom in LMonVar can be a constant value or a variable reference. */
  enum Atom:
    case Constant(n: Long)
    case Variable(name: String)

  /** An expression in LMonVar can be a constant value, a unary operation, a
    * binary operation, or a function call with the restriction to only accept
    * atoms.
    */
  enum Expr:
    case UnaryOp(op: UnaryOperator, lhs: Atom, rhs: Atom)
    case BinaryOp(op: BinaryOperator, lhs: Atom, rhs: Atom)
    case Call(name: String, args: List[Atom])
    case AtomExpr(a: Atom)

}
