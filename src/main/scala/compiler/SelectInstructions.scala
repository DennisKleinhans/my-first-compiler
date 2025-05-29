package compiler

import CommonNodes.*
import LMonVar.Expr.AtomExpr
import LMonVar.Atom

/** Converts an LMonVar atom to an x86Var argument.
  *
  * @param atom
  *   the LMonVar atom to convert
  * @return
  *   the corresponding x86Var argument either as an immediate value or a
  *   variable
  */
def argFromAtom(atom: LMonVar.Atom): x86Var.Arg = atom match
  case Atom.Constant(n)  => x86Var.Immediate(n)
  case Atom.Variable(id) => x86Var.Variable(id)

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
def selectInstructions(module: LMonVar.Module): List[x86Var.Instr] =
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
def selectInstructions(stmt: LMonVar.Stmt): List[x86Var.Instr] = stmt match
  case LMonVar.AssignStmt(id, expr) =>
    expr match
      case AtomExpr(atom) =>
        List(x86Var.MovQ(argFromAtom(atom), x86Var.Variable(id)))
      case LMonVar.UnaryOp(op, atom) =>
        op match
          case UnaryOperator.USub =>
            List(
              x86Var.MovQ(argFromAtom(atom), x86Var.Variable(id)),
              x86Var.NegQ(x86Var.Variable(id))
            )
      case LMonVar.BinaryOp(op, leftAtom, rightAtom) =>
        op match
          case BinaryOperator.Add =>
            List(
              x86Var.MovQ(argFromAtom(leftAtom), x86Var.Variable(id)),
              x86Var.AddQ(argFromAtom(rightAtom), x86Var.Variable(id))
            )
          case BinaryOperator.Sub =>
            List(
              x86Var.MovQ(argFromAtom(leftAtom), x86Var.Variable(id)),
              x86Var.SubQ(argFromAtom(rightAtom), x86Var.Variable(id))
            )
      // at the moment we only support the function read_int
      case LMonVar.Call(funId, args) if funId.name == "read_int" =>
        List(
          x86Var.CallQ("read_int", 0),
          x86Var.MovQ(x86Var.Register(x86.Reg.Rax), x86Var.Variable(id))
        )
      case _ => Nil

  case LMonVar.PrintStmt(a) =>
    List(
      x86Var.MovQ(argFromAtom(a), x86Var.Register(x86.Reg.Rdi)),
      x86Var.CallQ("print_int", 1)
    )
  case LMonVar.ExprStmt(e) =>
    e match
      // Evaluate a call expression for its side effects only (e.g., read_int()).
      // The result of the call (returned in RAX) is not stored or used.
      // This is intentional: ExprStmt(Call(...)) means the result is discarded.
      case LMonVar.Call(funId, args) if funId.name == "read_int" =>
        List(
          x86Var.CallQ("read_int", 0)
        )
      case _ => Nil
