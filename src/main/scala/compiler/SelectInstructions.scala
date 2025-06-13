package compiler

import CommonNodes.*
import CIf.AtomExpr
import x86.ByteReg
import compiler.CIf.Tail
import x86.Reg
import compiler.x86VarIf.Immediate

/** Converts an atom to an x86Var argument.
  *
  * @param atom
  *   the atom to convert
  * @return
  *   the corresponding x86Var argument either as an immediate numeric/boolean
  *   value or a variable
  */
def argFromAtom(atom: Atom): x86VarIf.Arg = atom match
  case Atom.Constant(n)     => x86VarIf.Immediate(n)
  case Atom.Variable(id)    => x86VarIf.Variable(id)
  case Atom.ConstantBool(b) => x86VarIf.Immediate(if b then 1 else 0)

/** Convert a CommonNodes.CompareOperator to an x86.Cc (condition code).
  *
  * @param op
  *   the comparison operator to convert
  * @return
  *   the corresponding x86 condition code
  */
def ccFromCompareOperator(op: CommonNodes.CompareOperator): x86.Cc = op match
  case CompareOperator.Eq    => x86.Cc.E
  case CompareOperator.NotEq => x86.Cc.NE
  case CompareOperator.Lt    => x86.Cc.L
  case CompareOperator.LtE   => x86.Cc.Le
  case CompareOperator.Gt    => x86.Cc.G
  case CompareOperator.GtE   => x86.Cc.Ge

/** Convert a CIf.CProgram (map from label → BasicBlock) into an
  * x86VarIf.Program. Each BasicBlock’s statements and tail are lowered to
  * x86VarIf.Instr, and then collected under the same label in the result.
  *
  * @param program
  *   the CIf program to compile
  * @return
  *   a x86VarIf.Program containing the Map of lables to blocks
  */
def selectInstructions(program: CIf.CProgram): x86VarIf.Program =
  val selectedBlocks: Map[String, List[x86VarIf.Instr]] = program.blocks.map {
    (lable, basicBlock) =>
      val stmtsInstrs = basicBlock.stmts.flatMap(lowerStmt)
      val tailInstrs = lowerTail(basicBlock.tail)
      lable -> (stmtsInstrs ++ tailInstrs)
  }
  x86VarIf.Program(selectedBlocks)

/** Lower a single CIf.Stmt into a sequence of x86VarIf.Instr. Handles
  * AssignStmt (Atom, UnaryNumericOp, BinaryNumericOp, Compare, ReadIntCall),
  * PrintStmt, and ExprStmt(ReadIntCall). Other cases produce no instructions.
  *
  * @param stmt
  *   the CIf statement to compile
  * @return
  *   a list of x86 instructions implementing the statement
  */
def lowerStmt(stmt: CIf.Stmt): List[x86VarIf.Instr] = stmt match
  case CIf.AssignStmt(id, expr) =>
    expr match
      case AtomExpr(atom) =>
        List(x86VarIf.MovQ(argFromAtom(atom), x86VarIf.Variable(id)))
      case CIf.UnaryNumericOp(op, atom) =>
        op match
          case UnaryNumericOperator.USub =>
            List(
              x86VarIf.MovQ(argFromAtom(atom), x86VarIf.Variable(id)),
              x86VarIf.NegQ(x86VarIf.Variable(id))
            )
      case CIf.BinaryNumericOp(op, leftAtom, rightAtom) =>
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

      // If we could eliminate this case here, we could produce better code an discard unnecessary assignments to tmp variables
      case CIf.Compare(cmp, e1, e2) =>
        val cc = ccFromCompareOperator(cmp)
        val src = argFromAtom(e2)
        val dest = argFromAtom(e1)
        List(
          x86VarIf.CmpQ(src, dest),
          x86VarIf.Set(cc, ByteReg.Al),
          x86VarIf.MovZBQ(ByteReg.Al, x86VarIf.Variable(id))
        )

      // at the moment we only support the function read_int
      case CIf.ReadIntCall =>
        List(
          x86VarIf.CallQ("read_int", 0),
          x86VarIf.MovQ(x86.Reg.Rax, x86VarIf.Variable(id))
        )

      case _ => Nil

  case CIf.PrintStmt(a) =>
    List(
      x86VarIf.MovQ(argFromAtom(a), x86.Reg.Rdi),
      x86VarIf.CallQ("print_int", 1)
    )

  case CIf.ExprStmt(e) =>
    e match
      // Evaluate a call expression for its side effects only (e.g., read_int()).
      // The result of the call (returned in RAX) is not stored or used.
      // This is intentional: ExprStmt(Call(...)) means the result is discarded.
      case CIf.ReadIntCall =>
        List(
          x86VarIf.CallQ("read_int", 0)
        )
      case _ => Nil

/** Lower a CIf.Tail into a sequence of x86VarIf.Instr. Handles Return (moving
  * the return value to RAX and jumping to "conclusion"), Goto (unconditional
  * jump), and If(Compare, thenGoto, elseGoto) (single cmp + conditional jump +
  * unconditional jump).
  *
  * @param tail
  *   the CIf tail to compile
  * @return
  *   a list of x86 instructions implementing the tail
  */
def lowerTail(tail: CIf.Tail): List[x86VarIf.Instr] = tail match
  case CIf.Return(e) =>
    e match
      case CIf.AtomExpr(atom) =>
        List(
          x86VarIf.MovQ(argFromAtom(atom), Reg.Rax),
          x86VarIf.Jmp("conclusion")
        )
      case _ =>
        List(
          x86VarIf.MovQ(x86VarIf.Immediate(0L), Reg.Rax),
          x86VarIf.Jmp("conclusion")
        )

  case CIf.Goto(lable) => List(x86VarIf.Jmp(lable))

  case CIf.If(cmp, thenGoto, elseGoto) =>
    val cc = ccFromCompareOperator(cmp.cmp)
    val src = argFromAtom(cmp.rhs)
    val dest = argFromAtom(cmp.lhs)
    List(
      x86VarIf.CmpQ(src, dest),
      x86VarIf.JmpIf(cc, thenGoto.lable),
      x86VarIf.Jmp(elseGoto.lable)
    )
