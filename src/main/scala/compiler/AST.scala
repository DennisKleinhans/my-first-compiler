package compiler

import compiler.CommonNodes.*
import compiler.LMonVar.Atom

object CommonNodes {

  /** A unary operator represents an operation with a single operand. */
  enum UnaryOperator:
    case USub

  /** A binary operator represents an operation with two operands. */
  enum BinaryOperator:
    case Add
    case Sub

  case class Identifier(name: String)
}

/** The abstract syntax tree (AST) for the LVar language.
  *
  * Represents the structure of LVar programs after parsing and before
  * interpretation or compilation.
  */
object LVar {
  export Expr.*
  export Stmt.*

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
    case AssignStmt(id: Identifier, e: Expr)

  /** An expression in LVar can be a constant value, a unary operation, a binary
    * operation, or a function call.
    */
  enum Expr:
    case Constant(n: Long)
    case UnaryOp(op: UnaryOperator, e: Expr)
    case BinaryOp(op: BinaryOperator, left: Expr, right: Expr)
    case Call(id: Identifier, args: List[Expr])
    case Variable(id: Identifier)

}

/** The abstract syntax tree (AST) for the LMonVar language.
  *
  * Represents the structure of LMonVar programs after parsing and before
  * interpretation or compilation.
  */
object LMonVar {
  export Expr.*
  export Stmt.*
  export Atom.*

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
    case AssignStmt(id: Identifier, rhs: Expr)
    case PrintStmt(a: Atom)
    case ExprStmt(e: Expr)

  /** An expression in LMonVar can be a constant value, a unary operation, a
    * binary operation, or a function call with the restriction to only accept
    * atoms.
    */
  enum Expr:
    case UnaryOp(op: UnaryOperator, a: Atom)
    case BinaryOp(op: BinaryOperator, lhs: Atom, rhs: Atom)
    case Call(id: Identifier, args: List[Atom])
    case AtomExpr(a: Atom)

  /** An atom in LMonVar can be a constant value or a variable reference. */
  enum Atom:
    case Constant(n: Long)
    case Variable(id: Identifier)
}

/** The x86Var intermediate representation (IR) for the LMonVar language.
  *
  * Represents the structure of LMonVar programs after instruction selection and
  * before final assembly generation.
  */
object x86Var {
  export Instr.*

  /*
   * An identifier in x86Var represents a variable name or label.
   * It is used to refer to variables in the x86Var instructions.
   */
  case class Variable(id: Identifier)

  /*
   * An immediate value in x86Var represents a constant integer that can be
   * used directly in instructions.
   */
  case class Immediate(n: Long)

  /** A location in x86Var can be a register or a variable.
    *
    * Registers are used for fast access to data, while variables represent
    * memory locations that may require stack allocation.
    */
  type Location = x86.Reg | Variable

  /** An argument in x86Var can be a location (register or variable) or an
    * immediate value (constant).
    *
    * This allows instructions to operate on both memory locations and immediate
    * values.
    */
  type Arg = Location | Immediate

  /** An instruction in x86Var represents a single operation in the x86
    * architecture. It can be a move, arithmetic operation, or a function call.
    */
  enum Instr:
    case MovQ(src: Arg, dest: Location)
    case AddQ(src: Arg, dest: Location)
    case SubQ(src: Arg, dest: Location)
    case NegQ(arg: Location)
    case CallQ(label: String, arity: Int)
    case PushQ(arg: Arg)
    case PopQ(arg: Arg)
    case RetQ
}
