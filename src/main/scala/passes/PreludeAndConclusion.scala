package passes

import x86.*
import x86.Instr
import x86.Immediate
import passes.RegisterAllocation.calleeSavedRegisters

object PreludeAndConclusion {

  def generatePreludeAndConclusion(
      progWithFuns: List[(String, x86.Program, Long, Set[x86.Reg])]
  ): x86.Program = {
    val program = progWithFuns
      .map { case (name, program, stackSpace, usedCalleSaved) =>
        generatePreludeAndConclusion(name, program, stackSpace, usedCalleSaved)
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
      stackSpace: Long,
      usedCalleeSavedRegisters: Set[x86.Reg]
  ): x86.Program =
    val x86.Program(blocks) = program
    val alignedSpace = computeAlignedStackSpace(stackSpace, usedCalleeSavedRegisters.size)

    val calleeSavedPushes = 
      usedCalleeSavedRegisters.toList.map(reg => Instr.PushQ(reg))

    val calleeSavedPops = 
      usedCalleeSavedRegisters.toList.map(reg => Instr.PopQ(reg)).reverse

    val prelude =
      List(
        Instr.PushQ(Reg.Rbp),
        Instr.MovQ(Reg.Rsp, Reg.Rbp)) ++ 
        calleeSavedPushes ++
        List(Instr.SubQ(Immediate(alignedSpace), Reg.Rsp)
      ) ++ (if (funName == "main") List(Instr.CallQ("initialize", 0))
            else Nil) ++ List(
        Instr.Jmp(funName + "_start")
      )

    val conclusionBlock = List(
      Instr.AddQ(Immediate(alignedSpace), Reg.Rsp)) ++
      calleeSavedPops ++
      List(Instr.PopQ(Reg.Rbp),
      Instr.RetQ)
    

    val updatedBlocks = blocks ++ Map(
      funName -> prelude,
      funName + "_conclusion" -> conclusionBlock
    )

    x86.Program(updatedBlocks)

  def computeAlignedStackSpace(stackSpace: Long, numPushes: Int): Long =
    val bytesPushed = numPushes * 8
    val baseAligned = ((stackSpace + 15) / 16) * 16
    val misalignment = (bytesPushed + baseAligned) % 16
    val fix = if misalignment == 8 then 0 else 8  
    baseAligned + fix
}
