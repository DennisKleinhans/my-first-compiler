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
  case Instr.AddQ(src, dest)     => f"addq ${format(src)}, ${format(dest)}"
  case Instr.SubQ(src, dest)     => f"subq ${format(src)}, ${format(dest)}"
  case Instr.NegQ(arg)           => f"negq ${format(arg)}"
  case Instr.IMulQ(src, dest)    => f"imulq ${format(src)}, ${format(dest)}"
  case Instr.MovQ(src, dest)     => f"movq ${format(src)}, ${format(dest)}"
  case Instr.PushQ(arg)          => f"pushq ${format(arg)}"
  case Instr.PopQ(arg)           => f"popq ${format(arg)}"
  case Instr.CallQ(label, _)     => f"callq $label"
  case Instr.IndCallQ(arg, _)    => f"callq *${format(arg)}"
  case Instr.RetQ                => "retq"
  case Instr.XorQ(src, dest)     => f"xorq ${format(src)}, ${format(dest)}"
  case Instr.CmpQ(lower, higher) => f"cmpq ${format(lower)}, ${format(higher)}"
  case Instr.MovZBQ(src, dest)   => f"movzbq ${format(src)}, ${format(dest)}"
  case Instr.LeaQ(arg, dest)     => f"leaq ${format(arg)}, ${format(dest)}"
  case Instr.Jmp(label)          => f"jmp $label"
  case Instr.JmpIf(cc, label)    => f"j${format(cc)} $label"
  case Instr.Set(cc, dest)       => f"set${format(cc)} ${format(dest)}"
  case Instr.TailJmp(arg, _)     => f"jmp *${format(arg)}"
  case Instr.AndQ(src, dest)     => f"andq ${format(src)}, ${format(dest)}"
  case Instr.SarQ(src,dest)      => f"sarq ${format(src)}, ${format(dest)}"
  case Instr.SalQ(src, dest)     => f"salq ${format(src)}, ${format(dest)}"
  case Instr.OrQ(src, dest)      => f"orq ${format(src)}, ${format(dest)}"
}

def format(cc: Cc): String = cc match {
  case Cc.E  => "e"
  case Cc.L  => "l"
  case Cc.Le => "le"
  case Cc.G  => "g"
  case Cc.Ge => "ge"
  case Cc.NE => "ne"
}

def format(a: Arg): String = a match {
  case Immediate(i)       => f"$$$i"
  case reg: Reg           => f"%%${format(reg)}"
  case Deref(reg, offset) => f"$offset(%%${format(reg)})"
  case reg: ByteReg       => f"%%${format(reg)}"
  case Global(label)      => f"$label(%%rip)"
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
  case Reg.R8  => "r8"
  case Reg.R9  => "r9"
  case Reg.R10 => "r10"
  case Reg.R11 => "r11"
  case Reg.R12 => "r12"
  case Reg.R13 => "r13"
  case Reg.R14 => "r14"
  case Reg.R15 => "r15"
}

def format(b: ByteReg): String = b match {
  case ByteReg.Ah => "ah"
  case ByteReg.Al => "al"
  case ByteReg.Bh => "bh"
  case ByteReg.Bl => "bl"
  case ByteReg.Ch => "ch"
  case ByteReg.Cl => "cl"
  case ByteReg.Dh => "dh"
  case ByteReg.Dl => "dl"
}
