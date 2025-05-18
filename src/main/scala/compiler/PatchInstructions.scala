package compiler

import x86.Instr
import x86.Instr.*
import x86.Reg.*
import x86.Arg.*

/** Rewrites instructions to avoid illegal memory-to-memory operations by
  * introducing the RAX register as an intermediate.
  *
  * @param instrs
  *   The list of x86 instructions to patch.
  * @return
  *   A list of x86 instructions with patched memory-to-memory moves.
  */
def patchInstructions(instrs: List[x86.Instr]): List[x86.Instr] =
  instrs.flatMap {
    case MovQ(src @ Deref(_, _), dest @ Deref(_, _)) =>
      List(MovQ(src, Register(Rax)), MovQ(Register(Rax), dest))
    case AddQ(src @ Deref(_, _), dest @ Deref(_, _)) =>
      List(MovQ(src, Register(Rax)), AddQ(Register(Rax), dest))
    case SubQ(src @ Deref(_, _), dest @ Deref(_, _)) =>
      List(MovQ(src, Register(Rax)), SubQ(Register(Rax), dest))
    case NegQ(arg @ Deref(_, _)) =>
      List(
        MovQ(arg, Register(Rax)),
        NegQ(Register(Rax)),
        MovQ(Register(Rax), arg)
      )
    case other => List(other)
  }
