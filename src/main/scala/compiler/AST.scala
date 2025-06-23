package compiler

import scala.compiletime.ops.long

object CommonNodes {

  /** An atom in can be a constant value (Long or Boolean) or a variable
    * reference.
    */
  enum Atom:
    case Constant(n: Long)
    case ConstantBool(b: Boolean)
    case Variable(id: Identifier)

  /** A unary operator for numerics represents an operation with a single
    * operand which must be a numeric.
    */
  enum UnaryNumericOperator:
    case USub

  /** A unary operator for booleans represents an operation with a single
    * operand which must be a boolean.
    */
  enum UnaryLogicOperator:
    case Not

  /** A binary operator for numerics represents an operation with two operands
    * which must be numeric.
    */
  enum BinaryNumericOperator:
    case Add
    case Sub

  /** A binary operator for booleans represents an operation with two operands
    * which must be booleans.
    */
  enum BinaryLogicOperator:
    case And
    case Or

  /** A comparison operator represents a binary operation that compares two
    * values.
    */
  enum CompareOperator:
    case Eq
    case NotEq
    case Lt
    case LtE
    case Gt
    case GtE

  case class Identifier(name: String)
}

/** The abstract syntax tree (AST) for the LWhile language.
  *
  * Represents the structure of LWhile programs after parsing and before
  * interpretation or compilation.
  */
object LCore {
  import CommonNodes.*
  export Expr.*
  export Stmt.*

  /** A module is the top-level program structure in LWhile.
    *
    * @param stmts
    *   the list of statements contained in the module
    */
  case class Module(stmts: List[Stmt])

  /** A statement in LWhile represents either a standalone expression, a print
    * operation, an assign operation, a if Statement or a while statement.
    */
  enum Stmt:
    case ExprStmt(e: Expr)
    case PrintStmt(e: Expr)
    case AssignStmt(id: Identifier, e: Expr)
    case IfStmt(cond: Expr, thenBranch: List[Stmt], elseBranch: List[Stmt])
    case WhileStmt(cond: Expr, body: List[Stmt])

  /** An expression in LWhile can be a constant value (Long or Boolean), a
    * Variable, a unary operation, a binary operation a function call, a compare
    * operation or a if expression.
    */
  enum Expr:
    case Constant(n: Long)
    case ConstantBool(b: Boolean)
    case UnaryNumericOp(op: UnaryNumericOperator, e: Expr)
    case UnaryLogicOp(op: UnaryLogicOperator, e: Expr)
    case BinaryNumericOp(op: BinaryNumericOperator, lhs: Expr, rhs: Expr)
    case BinaryLogicOp(op: BinaryLogicOperator, lhs: Expr, rhs: Expr)
    case Compare(cmp: CompareOperator, lhs: Expr, rhs: Expr)
    case ReadIntCall
    case Variable(id: Identifier)
    case IfExpr(condExpr: Expr, thenExpr: Expr, elseExpr: Expr)
}

/** The abstract syntax tree (AST) for the LMonWhile language.
  *
  * Represents the structure of LMonWhile programs after parsing and before
  * interpretation or compilation.
  */
object LMon {
  import CommonNodes.*
  import CommonNodes.Atom
  export Expr.*
  export Stmt.*

  /** A module is the top-level program structure in LMonWhile.
    *
    * @param stmts
    *   the list of statements contained in the module
    */
  case class Module(stmts: List[Stmt])

  /** A statement in LMonWhile represents either a standalone expression, a
    * print operation, an assign operation, a if statement or a while statement.
    */
  enum Stmt:
    case AssignStmt(id: Identifier, rhs: Expr)
    case PrintStmt(a: Atom)
    case ExprStmt(e: Expr)
    case IfStmt(cond: Expr, thenBranch: List[Stmt], elseBranch: List[Stmt])
    case WhileStmt(cond: Expr, body: List[Stmt])

  /** An expression in LMonWhile can be a constant value (Long or Boolean), a
    * unary operation, a binary operation, a function call with the restriction
    * to only accept atoms or a if expression also with the restiction to only
    * accept atoms.
    */
  enum Expr:
    case Constant(n: Long)
    case ConstantBool(b: Boolean)
    case UnaryNumericOp(op: UnaryNumericOperator, a: Atom)
    case UnaryLogicOp(op: UnaryLogicOperator, e: Expr)
    case BinaryNumericOp(op: BinaryNumericOperator, lhs: Atom, rhs: Atom)
    case Compare(cmp: CompareOperator, lhs: Atom, rhs: Atom)
    case ReadIntCall
    case AtomExpr(a: Atom)
    case IfExpr(condExpr: Expr, thenExpr: Expr, elseExpr: Expr)
    case Begin(stmts: List[Stmt], e: Expr)
}

/** The abstract syntax tree (AST) for the CIf language.
  *
  * Represents the structure of CIf programs after parsing and before
  * interpretation or compilation.
  *
  * This AST is designed to represent a simplified version of C-like programming
  * constructs, focusing on expressions, statements, and control flow with
  * if-else constructs.
  */
object CIr {
  import CommonNodes.*
  import CommonNodes.Atom
  export Tail.*
  export Expr.*
  export Stmt.*

  type Label = String

  /** An expression in CIf can be a atom (Long, Boolean or Variable), a unary
    * numeric operation, a binary numeric operation, a function call with the
    * restriction to only accept atoms or a Compare expression also with the
    * restiction to only accept atoms.
    */
  enum Expr:
    case Constant(n: Long)
    case ConstantBool(b: Boolean)
    case AtomExpr(a: Atom)
    case UnaryNumericOp(op: UnaryNumericOperator, a: Atom)
    case BinaryNumericOp(op: BinaryNumericOperator, lhs: Atom, rhs: Atom)
    case Compare(cmp: CompareOperator, lhs: Atom, rhs: Atom)
    case ReadIntCall

  /** A statement in CIf represents either a standalone expression, a print
    * operation or an assign operation.
    */
  enum Stmt:
    case PrintStmt(a: Atom)
    case ExprStmt(e: Expr)
    case AssignStmt(id: Identifier, e: Expr)

  /** A tail in CIf represents either a return, jump via goto or a conditional
    * jump via goto.
    */
  enum Tail:
    case Return(e: Expr)
    case Goto(lable: String) // could also be an Identifier instat of String
    case If(cmp: Expr.Compare, thenGoto: Tail.Goto, elseGoto: Tail.Goto)

  /** A basic block in CIf consists of a list of statements and a tail.
    */
  case class BasicBlock(stmts: List[Stmt], tail: Tail)

  /** A C program in CIf represents a collection of blocks, where each block is
    * identified by a unique name (String) and contains a list of statements and
    * a tail.
    *
    * @param blocks
    */
  case class CProgram(
      blocks: Map[Label, BasicBlock]
  ) // also here we could use Identifier instead of String

}

/** The x86VarIf intermediate representation (IR) for the LMonIf language.
  *
  * Represents the structure of LMonIf programs after instruction selection and
  * before final assembly generation.
  */
object x86Var {
  import x86.Cc
  import CommonNodes.Identifier
  export Instr.*

  case class Program(blocks: Map[String, List[x86Var.Instr]])

  /*
   * An identifier in x86VarIf represents a variable name or label.
   * It is used to refer to variables in the x86VarIf instructions.
   */
  case class Variable(id: Identifier)

  /*
   * An immediate value in x86VarIf represents a constant integer that can be
   * used directly in instructions.
   */
  case class Immediate(n: Long)

  /** A location in x86VarIf can be a register or a variable.
    *
    * Registers are used for fast access to data, while variables represent
    * memory locations that may require stack allocation.
    */
  type Location = x86.Reg | x86.ByteReg | x86.Deref | x86.Global | Variable

  /** An argument in x86VarIf can be a location (register or variable) or an
    * immediate value (constant).
    *
    * This allows instructions to operate on both memory locations and immediate
    * values.
    */
  type Arg = Location | Immediate

  /** An instruction in x86VarIf represents a single operation in the x86
    * architecture. It can be a move, push, pop, ret, arithmetic operation,
    * logic operation, jumps or a function call.
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
    case XorQ(src: Arg, dest: Location)
    case CmpQ(lower: Arg, higher: Arg)
    case MovZBQ(src: Arg, dest: Location)
    case Jmp(label: String)
    case JmpIf(cc: x86.Cc, label: String)
    case Set(cc: x86.Cc, dest: Location)
    case TailJmp(arg: Arg, arity: Int)
    case AndQ(src: Arg, dest: Location)
    case SarQ(src: Arg, dest: Location)
}
