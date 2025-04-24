package x86

case class Program(blocks: Map[String, List[Instr]])

enum Instr {
  case AddQ(src: Arg, dest: Arg)
  case SubQ(src: Arg, dest: Arg)
  case MovQ(src: Arg, dest: Arg)
  case NegQ(arg: Arg)
  case PushQ(arg: Arg)
  case PopQ(arg: Arg)
  case CallQ(label: String, arity: Int)
  case RetQ
  case Jmp(label: String)
}

enum Arg {
  case Immediate(int: Long)
  case Register(reg: Reg)
  case Deref(reg: Reg, offset: Long)
}

enum Reg {

  /**
   * Stack Pointer - Points to the top of the stack
   */
  case Rsp

  /**
   * Base Pointer - Frame pointer for stack frames
   */
  case Rbp

  /**
   * Accumulator - Used for function return values
   */
  case Rax

  /**
   * Base Register - General purpose (callee-saved)
   */
  case Rbx

  /**
   * Counter Register - 4th argument in calling convention (Used for loops and string operations)
   */
  case Rcx

  /**
   * Data Register - 3rd argument in calling convention
   */
  case Rdx

  /**
   * Source Index - 2nd argument in calling convention
   */
  case Rsi

  /**
   * Destination Index - 1st argument in calling convention
   */
  case Rdi

  /**
   * General purpose register - 5th argument in calling convention
   */
  case R8

  /**
   * General purpose register - 6th argument in calling convention
   */
  case R9

  /**
   * General purpose register (caller-saved)
   */
  case R10

  /**
   * General purpose register (caller-saved)
   */
  case R11

  /**
   * General purpose register (caller-saved)
   */
  case R12

  /**
   * General purpose register (caller-saved)
   */
  case R13

  /**
   * General purpose register (caller-saved)
   */
  case R14

  /**
   * General purpose register (caller-saved)
   */
  case R15
}
