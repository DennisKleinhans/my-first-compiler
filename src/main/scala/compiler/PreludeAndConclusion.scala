package compiler

import x86.*
import x86.Instr
import x86.Instr.*
import x86.Immediate

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
