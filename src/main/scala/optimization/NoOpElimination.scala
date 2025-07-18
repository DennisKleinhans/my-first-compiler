package optimization

import x86.Instr
import x86.Reg

object NoOpElimination {

  def eliminateNoOps(
      progWithFuns: List[(String, x86.Program, Long, Set[Reg])]
  ): List[(String, x86.Program, Long, Set[Reg])] = {
    progWithFuns.map { case (name, program, stackSpace, usedCalleSaved) =>
      (name, eliminateNoOps(program), stackSpace, usedCalleSaved)
    }
  }

  def eliminateNoOps(program: x86.Program): x86.Program = {
    val filteredBlocks = program.blocks.map { case (lable, instrs) =>
      val cleaned = instrs.filter {
        case Instr.MovQ(src: Reg, dest: Reg) if src == dest => false
        case _                                        => true
      }
      lable -> cleaned
    }
    x86.Program(filteredBlocks)
  }
}
