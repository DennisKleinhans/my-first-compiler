package compiler

import x86.Instr
import x86.Instr.*
import x86.Arg.*
import x86.Reg.*

def preludeAndConclusion(
    instrs: List[x86.Instr],
    stackSpace: Int
): List[x86.Instr] =
  val alignedSpace = ((stackSpace + 15) / 16) * 16

  val prolog = List(
    PushQ(Register(Rbp)),
    MovQ(Register(Rsp), Register(Rbp)),
    SubQ(Immediate(alignedSpace), Register(Rsp))
  )
  val epilog = List(
    AddQ(Immediate(alignedSpace), Register(Rsp)),
    PopQ(Register(Rbp)),
    MovQ(Immediate(0), Register(Rax)),
    RetQ
  )
  prolog ++ instrs ++ epilog
