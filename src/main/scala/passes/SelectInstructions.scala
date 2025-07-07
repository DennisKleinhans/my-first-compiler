package passes

import compiler.{CommonNodes, CIr, x86Var}
import CIr.AtomExpr
import x86.ByteReg
import CIr.Tail
import x86.Reg
import x86Var.Immediate
import CommonNodes.*
import compiler.x86Var.Variable

object SelectInstructions {

  /** Converts an atom to an x86Var argument.
    *
    * @param atom
    *   the atom to convert
    * @return
    *   the corresponding x86Var argument either as an immediate numeric/boolean
    *   value or a variable
    */
  def argFromAtom(atom: Atom): x86Var.Arg = atom match
    case Atom.Constant(n)     => x86Var.Immediate(n)
    case Atom.Variable(id)    => x86Var.Variable(id)
    case Atom.ConstantBool(b) => x86Var.Immediate(if b then 1 else 0)

  def LocFromAtom(atom: Atom): x86Var.Location = argFromAtom(atom) match
    case x86Var.Variable(id) => x86Var.Variable(id)
    case other             => sys error f"expected a Variable, but got $other"

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
    case CompareOperator.Is =>
      x86.Cc.E // the Is comparsion compares the adresses of tuples, so we translate it simply to an equality comparsion regarding an adress comparsion

  /** Convert a CIr.CProgram (map from label → BasicBlock) into an
    * x86Var.Program. Each BasicBlock’s statements and tail are lowered to
    * x86Var.Instr, and then collected under the same label in the result.
    *
    * @param program
    *   the CIr program to compile
    * @return
    *   a x86Var.Program containing the Map of lables to blocks
    */
  def selectInstructions(program: CIr.CProgram): x86Var.Program =
    val selectedFunctions = program.funDefs.map(selectInstructions)
    x86Var.Program(selectedFunctions)


  def selectInstructions(funDef: CIr.FunctionDef): x86Var.FunctionDef =
    val selectedBlocks: Map[String, List[x86Var.Instr]] = funDef.body.map {
      (lable, basicBlock) =>
        val stmtsInstrs = basicBlock.stmts.flatMap(lowerStmt)
        val tailInstrs = lowerTail(basicBlock.tail)
        lable -> (stmtsInstrs ++ tailInstrs)
    }
    x86Var.FunctionDef(funDef.name, selectedBlocks)

  /** Lower a single CIr.Stmt into a sequence of x86Var.Instr. Handles
    * AssignStmt (Atom, UnaryNumericOp, BinaryNumericOp, Compare, ReadIntCall),
    * PrintStmt, and ExprStmt(ReadIntCall). Other cases produce no instructions.
    *
    * @param stmt
    *   the CIr statement to compile
    * @return
    *   a list of x86 instructions implementing the statement
    */
  def lowerStmt(stmt: CIr.Stmt): List[x86Var.Instr] = stmt match
    case CIr.AssignStmt(id, expr) =>
      expr match {
        case AtomExpr(atom) =>
          List(x86Var.MovQ(argFromAtom(atom), x86Var.Variable(id)))
        case CIr.UnaryNumericOp(op, atom) =>
          op match
            case UnaryNumericOperator.USub =>
              List(
                x86Var.MovQ(argFromAtom(atom), x86Var.Variable(id)),
                x86Var.NegQ(x86Var.Variable(id))
              )
        case CIr.BinaryNumericOp(op, leftAtom, rightAtom) =>
          op match
            case BinaryNumericOperator.Add =>
              List(
                x86Var.MovQ(argFromAtom(leftAtom), x86Var.Variable(id)),
                x86Var.AddQ(argFromAtom(rightAtom), x86Var.Variable(id))
              )
            case BinaryNumericOperator.Sub =>
              List(
                x86Var.MovQ(argFromAtom(leftAtom), x86Var.Variable(id)),
                x86Var.SubQ(argFromAtom(rightAtom), x86Var.Variable(id))
              )

        // If we could eliminate this case here, we could produce better code an discard unnecessary assignments to tmp variables
        case CIr.Compare(cmp, e1, e2) =>
          val cc = ccFromCompareOperator(cmp)
          val src = argFromAtom(e2)
          val dest = argFromAtom(e1)
          List(
            x86Var.CmpQ(src, dest),
            x86Var.Set(cc, ByteReg.Al),
            x86Var.MovZBQ(ByteReg.Al, x86Var.Variable(id))
          )

        // at the moment we only support the function read_int
        case CIr.ReadIntCall =>
          List(
            x86Var.CallQ("read_int", 0),
            x86Var.MovQ(x86.Reg.Rax, x86Var.Variable(id))
          )

        case CIr.Allocate(size) =>
          List(
            x86Var.MovQ(Immediate(size), Reg.Rdi),
            x86Var.CallQ("allocate", 1),
            x86Var.MovQ(Reg.Rax, x86Var.Variable(id))
          )

        case CIr.Load(ptr, offset) =>
          List(
           x86Var.LoadQ(Variable(id), argFromAtom(ptr).asInstanceOf[Variable], offset)
          )

        case _ => Nil
      }

    case CIr.PrintStmt(a) =>
      List(
        x86Var.MovQ(argFromAtom(a), x86.Reg.Rdi),
        x86Var.CallQ("print_int", 1)
      )

    case CIr.ExprStmt(e) =>
      e match
        // Evaluate a call expression for its side effects only (e.g., read_int()).
        // The result of the call (returned in RAX) is not stored or used.
        // This is intentional: ExprStmt(Call(...)) means the result is discarded.
        case CIr.ReadIntCall =>
          List(
            x86Var.CallQ("read_int", 0)
          )
        case _ => Nil

    case CIr.StoreStmt(ptr, offset, value) =>
      List(
        x86Var.StoreQ(argFromAtom(ptr).asInstanceOf[Variable], offset, argFromAtom(value))
      )

  /** Lower a CIr.Tail into a sequence of x86Var.Instr. Handles Return (moving
    * the return value to RAX and jumping to "conclusion"), Goto (unconditional
    * jump), and If(Compare, thenGoto, elseGoto) (single cmp + conditional jump
    * + unconditional jump).
    *
    * @param tail
    *   the CIr tail to compile
    * @return
    *   a list of x86 instructions implementing the tail
    */
  def lowerTail(tail: CIr.Tail): List[x86Var.Instr] = tail match
    case CIr.Return(e) =>
      e match
        case CIr.AtomExpr(atom) =>
          List(
            x86Var.MovQ(argFromAtom(atom), Reg.Rax),
            x86Var.Jmp("conclusion")
          )
        case _ =>
          List(
            x86Var.MovQ(x86Var.Immediate(0L), Reg.Rax),
            x86Var.Jmp("conclusion")
          )

    case CIr.Goto(lable) => List(x86Var.Jmp(lable))

    case CIr.If(cmp, thenGoto, elseGoto) =>
      val cc = ccFromCompareOperator(cmp.cmp)
      val src = argFromAtom(cmp.rhs)
      val dest = argFromAtom(cmp.lhs)
      List(
        x86Var.CmpQ(src, dest),
        x86Var.JmpIf(cc, thenGoto.lable),
        x86Var.Jmp(elseGoto.lable)
      )
}
