package compiler

import x86.*
import x86.Instr.*
import x86.Reg.*

def isTooBig(imm: Long): Boolean = imm > Int.MaxValue || imm < Int.MinValue

/** Rewrites instructions to avoid illegal memory-to-memory operations and
  * immediates >32-bit by introducing the RAX register as an intermediate.
  *
  * @param instrs
  *   The list of x86 instructions to patch.
  * @return
  *   A list of x86 instructions with patched memory-to-memory moves.
  */
def patchInstructions(instrs: List[x86.Instr]): List[x86.Instr] =
  instrs.flatMap {
    case MovQ(src @ Deref(_, _), dest @ Deref(_, _)) =>
      List(MovQ(src, Reg.Rax), MovQ(Reg.Rax, dest))
    case AddQ(src @ Deref(_, _), dest @ Deref(_, _)) =>
      List(MovQ(src, Reg.Rax), AddQ(Reg.Rax, dest))
    case SubQ(src @ Deref(_, _), dest @ Deref(_, _)) =>
      List(MovQ(src, Reg.Rax), SubQ(Reg.Rax, dest))
    case NegQ(arg @ Deref(_, _)) =>
      List(
        MovQ(arg, Reg.Rax),
        NegQ(Reg.Rax),
        MovQ(Reg.Rax, arg)
      )
    // also patch immediate values that are too big to fit in a 32-bit immediate
    case MovQ(imm @ Immediate(n), dest @ Deref(_, _)) if isTooBig(n) =>
      List(MovQ(imm, Reg.Rax), MovQ(Reg.Rax, dest))

    case AddQ(imm @ Immediate(n), dest @ Deref(_, _)) if isTooBig(n) =>
      List(MovQ(imm, Reg.Rax), AddQ(Reg.Rax, dest))

    case SubQ(imm @ Immediate(n), dest @ Deref(_, _)) if isTooBig(n) =>
      List(MovQ(imm, Reg.Rax), SubQ(Reg.Rax, dest))
    case other => List(other)
  }
