package passes

import x86.*
import x86.Instr
import x86.Instr.*
import x86.Immediate

object PreludeAndConclusion {

  def generatePreludeAndConclusion(
      progWithFuns: List[(String, x86.Program, Long)]
  ): x86.Program = {
    val program = progWithFuns
      .map { case (name, program, stackSpace) =>
        generatePreludeAndConclusion(name, program, stackSpace)
      }
      .flatMap(_.blocks)
      .groupBy(_._1)
      .view
      .mapValues(_.flatMap(_._2))
      .toMap
    x86.Program(program)
  }

  /** Adds a prelude and conclusion to the x86 program. The prelude sets up the
    * stack frame and jumps to the "start" label. The conclusion restores the
    * stack frame and returns from the main function.
    *
    * @param program
    *   The x86 program to which the prelude and conclusion will be added.
    * @param stackSpace
    *   The amount of stack space needed for the program, in bytes. This will be
    *   aligned to the nearest 16 bytes.
    * @return
    *   A new x86 program with the prelude and conclusion added.
    */
  def generatePreludeAndConclusion(
      funName: String,
      program: x86.Program,
      stackSpace: Long
  ): x86.Program =
    val x86.Program(blocks) = program
    val alignedSpace =
      if stackSpace == 0 then 16 else ((stackSpace + 15) / 16) * 16

    val prelude =
      List(
        PushQ(Reg.Rbp),
        MovQ(Reg.Rsp, Reg.Rbp),
        SubQ(Immediate(alignedSpace), Reg.Rsp)
      ) ++ (if (funName == "main") List(CallQ("initialize", 0))
            else Nil) ++ List(
        Jmp(funName + "_start")
      )

    val conclusionBlock = List(
      AddQ(Immediate(alignedSpace), Reg.Rsp),
      PopQ(Reg.Rbp),
      RetQ
    )

    val updatedBlocks = blocks ++ Map(
      funName -> prelude,
      funName + "_conclusion" -> conclusionBlock
    )

    x86.Program(updatedBlocks)
}
