package passes

import x86.*
import x86.Instr.*
import x86.Reg.*
import x86.ByteReg

object PatchInstructions {

  def isTooBig(imm: Long): Boolean = imm > Int.MaxValue || imm < Int.MinValue

  /** Patches a x86 program to avoid illegal memory-to-memory operations and
    * immediates >32-bit by introducing the RAX register as an intermediate.
    *
    * @param program
    *   The x86 program to patch.
    * @return
    *   A patched x86 program with memory-to-memory moves replaced by two-step
    *   moves through RAX.
    */
  def patchInstructions(program: x86.Program): x86.Program = {

    val patchedBlocks = program.blocks.map { (label, instrs) =>
      label -> patchInstructions(instrs)
    }
    x86.Program(patchedBlocks)
  }

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

      case MovZBQ(src: ByteReg, dest @ Deref(_, _)) =>
        List(MovZBQ(src, Reg.Rax), MovQ(Reg.Rax, dest))

      // also patch immediate values that are too big to fit in a 32-bit immediate
      case MovQ(imm @ Immediate(n), dest @ Deref(_, _)) if isTooBig(n) =>
        List(MovQ(imm, Reg.Rax), MovQ(Reg.Rax, dest))

      case AddQ(imm @ Immediate(n), dest @ Deref(_, _)) if isTooBig(n) =>
        List(MovQ(imm, Reg.Rax), AddQ(Reg.Rax, dest))

      case SubQ(imm @ Immediate(n), dest @ Deref(_, _)) if isTooBig(n) =>
        List(MovQ(imm, Reg.Rax), SubQ(Reg.Rax, dest))

      case CmpQ(lower @ Immediate(n), higher @ Deref(_, _)) if isTooBig(n) =>
        List(MovQ(lower, Reg.Rax), CmpQ(Reg.Rax, higher))

      case CmpQ(lower @ Deref(_, _), higher @ Deref(_, _)) =>
        List(MovQ(lower, Reg.Rax), CmpQ(Reg.Rax, higher))

      case CmpQ(Immediate(n1), Immediate(n2)) =>
        List(MovQ(Immediate(n2), Reg.Rax), CmpQ(Immediate(n1), Reg.Rax))

      case other => List(other)
    }
}
