package compiler

import CommonNodes.*
import LMonIf.Expr.AtomExpr

/** Converts an LMonVar atom to an x86Var argument.
  *
  * @param atom
  *   the LMonVar atom to convert
  * @return
  *   the corresponding x86Var argument either as an immediate value or a
  *   variable
  */
def argFromAtom(atom: Atom): x86VarIf.Arg = atom match
  case Atom.Constant(n)  => x86VarIf.Immediate(n)
  case Atom.Variable(id) => x86VarIf.Variable(id)

/** Translates an entire LMonVar module into a flat list of x86 instructions.
  *
  * This function maps over all top-level statements in the module and applies
  * instruction selection to each of them.
  *
  * @param module
  *   the LMonVar module to compile
  * @return
  *   a list of x86 instructions representing the module
  */
def selectInstructions(module: LMonIf.Module): List[x86VarIf.Instr] =
  module.stmts.flatMap(selectInstructions)

/** Translates a single LMonVar statement into a list of x86 instructions.
  *
  * This function implements instruction selection: it lowers high-level LMonVar
  * statements into architecture-specific instructions for the x86Var IR.
  *
  *   - Assignments are translated based on the structure of the right-hand
  *     expression.
  *   - Only simple unary/binary operations and the `read_int` function are
  *     supported.
  *   - Print statements generate a call to `print_int`.
  *   - Expression statements are only translated if they are calls (e.g.,
  *     `read_int()`) used for side effects, and their result is discarded.
  *
  * @param stmt
  *   the LMonVar statement to compile
  * @return
  *   a list of x86 instructions implementing the statement
  */
def selectInstructions(stmt: LMonIf.Stmt): List[x86VarIf.Instr] = stmt match
  case LMonIf.AssignStmt(id, expr) =>
    expr match
      case AtomExpr(atom) =>
        List(x86VarIf.MovQ(argFromAtom(atom), x86VarIf.Variable(id)))
      case LMonIf.UnaryNumericOp(op, atom) =>
        op match
          case UnaryNumericOperator.USub =>
            List(
              x86VarIf.MovQ(argFromAtom(atom), x86VarIf.Variable(id)),
              x86VarIf.NegQ(x86VarIf.Variable(id))
            )
      case LMonIf.BinaryNumericOp(op, leftAtom, rightAtom) =>
        op match
          case BinaryNumericOperator.Add =>
            List(
              x86VarIf.MovQ(argFromAtom(leftAtom), x86VarIf.Variable(id)),
              x86VarIf.AddQ(argFromAtom(rightAtom), x86VarIf.Variable(id))
            )
          case BinaryNumericOperator.Sub =>
            List(
              x86VarIf.MovQ(argFromAtom(leftAtom), x86VarIf.Variable(id)),
              x86VarIf.SubQ(argFromAtom(rightAtom), x86VarIf.Variable(id))
            )
      // at the moment we only support the function read_int
      case LMonIf.ReadIntCall =>
        List(
          x86VarIf.CallQ("read_int", 0),
          x86VarIf.MovQ(x86.Reg.Rax, x86VarIf.Variable(id))
        )
      case _ => Nil

  case LMonIf.PrintStmt(a) =>
    List(
      x86VarIf.MovQ(argFromAtom(a), x86.Reg.Rdi),
      x86VarIf.CallQ("print_int", 1)
    )
  case LMonIf.ExprStmt(e) =>
    e match
      // Evaluate a call expression for its side effects only (e.g., read_int()).
      // The result of the call (returned in RAX) is not stored or used.
      // This is intentional: ExprStmt(Call(...)) means the result is discarded.
      case LMonIf.ReadIntCall =>
        List(
          x86VarIf.CallQ("read_int", 0)
        )
      case _ => Nil
