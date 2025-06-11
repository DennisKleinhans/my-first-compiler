package compiler

import x86.*
import x86.Instr
import x86.Instr.*
import x86.Immediate

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
def preludeAndConclusion(
    program: x86.Program,
    stackSpace: Int
): x86.Program =
  val x86.Program(blocks) = program
  val alignedSpace = ((stackSpace + 15) / 16) * 16

  val mainBlock = List(PushQ(Reg.Rbp)) ++
    List(
      MovQ(Reg.Rsp, Reg.Rbp),
      SubQ(Immediate(alignedSpace), Reg.Rsp),
      Jmp("start")
    )

  val conclusionBlock = List(
    AddQ(Immediate(alignedSpace), Reg.Rsp),
    PopQ(Reg.Rbp),
    RetQ
  )

  val updatedBlocks = blocks ++ Map(
    "main" -> mainBlock,
    "conclusion" -> conclusionBlock
  )

  x86.Program(updatedBlocks)
