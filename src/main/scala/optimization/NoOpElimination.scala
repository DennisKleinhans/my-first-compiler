package optimization

import x86.Instr.*
import x86.Reg

object NoOpElimination {

  def eliminateNoOps(
      progWithFuns: List[(String, x86.Program, Long)]
  ): List[(String, x86.Program, Long)] = {
    progWithFuns.map { case (name, program, stackSpace) =>
      (name, eliminateNoOps(program), stackSpace)
    }
  }

  def eliminateNoOps(program: x86.Program): x86.Program = {
    val filteredBlocks = program.blocks.map { case (lable, instrs) =>
      val cleaned = instrs.filter {
        case MovQ(src: Reg, dest: Reg) if src == dest => false
        case _                                        => true
      }
      lable -> cleaned
    }
    x86.Program(filteredBlocks)
  }
}
