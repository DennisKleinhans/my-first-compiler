package x86

case class Program(blocks: Map[String, List[Instr]])

enum Instr {
  case AddQ(src: Arg, dest: Location)
  case SubQ(src: Arg, dest: Location)
  case NegQ(arg: Location)
  case IMulQ(src: Arg, dest: Location)
  case MovQ(src: Arg, dest: Location)
  case PushQ(arg: Arg)
  case PopQ(arg: Arg)
  case CallQ(label: String, arity: Int)
  case IndCallQ(arg: Arg, arity: Int)
  case RetQ
  case XorQ(src: Arg, dest: Location)
  case CmpQ(lower: Arg, higher: Arg)
  case MovZBQ(src: Arg, dest: Location)
  case LeaQ(arg: Arg, reg: Reg)
  case Jmp(label: String)
  case JmpIf(cc: Cc, label: String)
  case Set(cc: Cc, dest: Location)
  case TailJmp(arg: Arg, arity: Int)
  case AndQ(src: Arg, dest: Location)
  case SarQ(src: Arg, dest: Location)
}

enum Cc {
  case E
  case NE
  case L
  case Le
  case G
  case Ge
}

case class Immediate(int: Long)
case class Deref(reg: Reg, offset: Long)
case class Global(label: String)

type Location = Reg | Deref | Global | ByteReg
type Arg = Location | Immediate

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

enum ByteReg {
  case Ah
  case Al
  case Bh
  case Bl
  case Ch
  case Cl
  case Dh
  case Dl
}

