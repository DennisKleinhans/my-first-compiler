package passes

import x86.*
import x86.Instr
import x86.Reg
import x86.ByteReg

object PatchInstructions {

  def isTooBig(imm: Long): Boolean = imm > Int.MaxValue || imm < Int.MinValue

  def patchProgWithFun(
      progWithFuns: List[(String, x86.Program, Long, Set[x86.Reg])]
  ): List[(String, x86.Program, Long, Set[x86.Reg])] = {
    val patchedProgWithFuns = progWithFuns.map {
      case (name, program, stackSpace, usedCalleSaved) =>
        (name, patchInstructionsProg(program), stackSpace, usedCalleSaved)
    }
    patchedProgWithFuns
  }

  /** Patches a x86 program to avoid illegal memory-to-memory operations and
    * immediates >32-bit by introducing the RAX register as an intermediate.
    *
    * @param program
    *   The x86 program to patch.
    * @return
    *   A patched x86 program with memory-to-memory moves replaced by two-step
    *   moves through RAX.
    */
  def patchInstructionsProg(program: x86.Program): x86.Program = {

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

      case Instr.MovQ(src @ Deref(_, _), dest @ Deref(_, _)) =>
        List(Instr.MovQ(src, Reg.Rax), Instr.MovQ(Reg.Rax, dest))

      case Instr.AddQ(src @ Deref(_, _), dest @ Deref(_, _)) =>
        List(Instr.MovQ(src, Reg.Rax), Instr.AddQ(Reg.Rax, dest))

      case Instr.SubQ(src @ Deref(_, _), dest @ Deref(_, _)) =>
        List(Instr.MovQ(src, Reg.Rax), Instr.SubQ(Reg.Rax, dest))

      case Instr.NegQ(arg @ Deref(_, _)) =>
        List(
          Instr.MovQ(arg, Reg.Rax),
          Instr.NegQ(Reg.Rax),
          Instr.MovQ(Reg.Rax, arg)
        )

      case Instr.MovZBQ(src: ByteReg, dest @ Deref(_, _)) =>
        List(Instr.MovZBQ(src, Reg.Rax), Instr.MovQ(Reg.Rax, dest))

      // also patch immediate values that are too big to fit in a 32-bit immediate
      case Instr.MovQ(imm @ Immediate(n), dest @ Deref(_, _)) if isTooBig(n) =>
        List(Instr.MovQ(imm, Reg.Rax), Instr.MovQ(Reg.Rax, dest))

      case Instr.AddQ(imm @ Immediate(n), dest @ Deref(_, _)) if isTooBig(n) =>
        List(Instr.MovQ(imm, Reg.Rax), Instr.AddQ(Reg.Rax, dest))

      case Instr.SubQ(imm @ Immediate(n), dest @ Deref(_, _)) if isTooBig(n) =>
        List(Instr.MovQ(imm, Reg.Rax), Instr.SubQ(Reg.Rax, dest))

      case Instr.CmpQ(lower @ Immediate(n), higher @ Deref(_, _)) if isTooBig(n) =>
        List(Instr.MovQ(lower, Reg.Rax), Instr.CmpQ(Reg.Rax, higher))

      case Instr.CmpQ(lower @ Deref(_, _), higher @ Deref(_, _)) =>
        List(Instr.MovQ(lower, Reg.Rax), Instr.CmpQ(Reg.Rax, higher))

      case Instr.CmpQ(Immediate(n1), Immediate(n2)) =>
        List(Instr.MovQ(Immediate(n2), Reg.Rax), Instr.CmpQ(Immediate(n1), Reg.Rax))

      case other => List(other)
    }
}
