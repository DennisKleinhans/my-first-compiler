package x86

def format(prog: Program): String = prog match {
  case Program(blocks) =>
    ".globl main\n" +
      blocks.flatMap { case (label, instrs) => f"\n$label:" :: instrs.map(format) }.mkString("\n")
}

def formatMacOS(prog: Program): String = prog match {
  case Program(blocks) =>
    ".globl _main\n" +
      "_main: jmp main\n" +
      blocks.flatMap { case (label, instrs) => f"\n$label:" :: instrs.map(format) }.mkString("\n")
}

def format(i: Instr): String = i match {
  case Instr.AddQ(src, dest) => f"addq ${format(src)}, ${format(dest)}"
  case Instr.SubQ(src, dest) => f"subq ${format(src)}, ${format(dest)}"
  case Instr.MovQ(src, dest) => f"movq ${format(src)}, ${format(dest)}"
  case Instr.NegQ(arg) => f"negq ${format(arg)}"
  case Instr.PushQ(arg) => f"pushq ${format(arg)}"
  case Instr.PopQ(arg) => f"popq ${format(arg)}"
  case Instr.CallQ(label, _) => f"callq $label"
  case Instr.RetQ => "retq"
  case Instr.Jmp(label) => f"jmp $label"
}

def format(a: Arg): String = a match {
  case Arg.Immediate(i) => f"$$$i"
  case Arg.Register(reg) => f"%%${format(reg)}"
  case Arg.Deref(reg, offset) => f"$offset(%%${format(reg)})"
}

def format(r: Reg): String = r match {
  case Reg.Rsp => "rsp"
  case Reg.Rbp => "rbp"
  case Reg.Rax => "rax"
  case Reg.Rbx => "rbx"
  case Reg.Rcx => "rcx"
  case Reg.Rdx => "rdx"
  case Reg.Rsi => "rsi"
  case Reg.Rdi => "rdi"
  case Reg.R8 => "r8"
  case Reg.R9 => "r9"
  case Reg.R10 => "r10"
  case Reg.R11 => "r11"
  case Reg.R12 => "r12"
  case Reg.R13 => "r13"
  case Reg.R14 => "r14"
  case Reg.R15 => "r15"
}
