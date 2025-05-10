package compiler

/** The abstract syntax tree (AST) for the LInt language.
  *
  * Represents the structure of LInt programs after parsing and before
  * interpretation or compilation.
  */
object AST {

  /** A module is the top-level program structure in LInt.
    *
    * @param stmts
    *   the list of statements contained in the module
    */
  case class Module(stmts: List[Stmt])

  /** A statement in LInt represents either a standalone expression or a print
    * operation.
    */
  enum Stmt:
    /** An expression statement that evaluates an expression but discards the
      * result.
      *
      * @param e
      *   the expression to evaluate
      */
    case ExprStmt(e: Expr)

    /** A print statement that evaluates an expression and prints the result.
      *
      * @param e
      *   the expression whose value will be printed
      */
    case PrintStmt(e: Expr)

    /** An assignment statement that assigns a value to a variable.
      *
      * @param name
      *   the name of the variable to assign to
      * @param e
      *   the expression whose value will be assigned
      */
    case AssignStmt(name: String, e: Expr)

  /** An expression in LInt can be a constant value, a unary operation, a binary
    * operation, or a function call.
    */
  enum Expr:
    /** A constant numeric literal.
      *
      * @param n
      *   the numeric value
      */
    case Constant(n: Long)

    /** A unary operation on an expression.
      *
      * @param op
      *   the unary operator
      * @param e
      *   the operand expression
      */
    case UnaryOp(op: UnaryOperator, e: Expr)

    /** A binary operation combining two expressions.
      *
      * @param op
      *   the binary operator
      * @param left
      *   the left operand
      * @param right
      *   the right operand
      */
    case BinaryOp(op: BinaryOperator, left: Expr, right: Expr)

    /** A function call expression.
      *
      * @param name
      *   the name of the function to call
      * @param args
      *   the arguments passed to the function
      */
    case Call(name: String, args: List[Expr])

    /** A variable reference.
      *
      * @param name
      *   the name of the variable
      */
    case Variable(name: String)

  /** A unary operator represents an operation with a single operand. */
  enum UnaryOperator:
    /** Negation (e.g., `-x`) */
    case USub

  /** A binary operator represents an operation with two operands. */
  enum BinaryOperator:
    /** Addition (e.g., `x + y`) */
    case Add

    /** Subtraction (e.g., `x - y`) */
    case Sub
}
