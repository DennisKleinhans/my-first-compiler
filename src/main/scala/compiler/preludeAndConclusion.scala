package compiler

import x86.*
import x86.Instr
import x86.Instr.*
import x86.Immediate

def preludeAndConclusion(
    instrs: List[x86.Instr],
    stackSpace: Int
): List[x86.Instr] =
  val alignedSpace = ((stackSpace + 15) / 16) * 16

  val prolog = List(
    PushQ(Reg.Rbp),
    MovQ(Reg.Rsp, Reg.Rbp),
    SubQ(Immediate(alignedSpace), Reg.Rsp)
  )
  val epilog = List(
    AddQ(Immediate(alignedSpace), Reg.Rsp),
    PopQ(Reg.Rbp),
    MovQ(Immediate(0), Reg.Rax),
    RetQ
  )
  prolog ++ instrs ++ epilog
