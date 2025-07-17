package compiler

import lang.SExp
import lang.SExp.*
import lang.parse
import x86.Program
import x86.format
import x86.assemble

import java.nio.file.{Path, Paths}
import scala.io.StdIn
import analysis.*
import passes.SelectInstructions.selectInstructions
import passes.ExplicateControl.explicateControl
import passes.RemoveComplexOperands.removeComplexOperands
import passes.Shrink.shrink
import passes.PatchInstructions.patchInstructions
import passes.PreludeAndConclusion.generatePreludeAndConclusion
import passes.RegisterAllocation.{allocateRegisters, basicblockGraph}
import optimization.NoOpElimination.eliminateNoOps
import passes.PatchInstructions.patchProgWithFun

@main
def main(): Unit =
  // val path = "examples/loop_compound.lang"
  // compile(Paths.get(path))

  val program1 =
    """
  def add(x : int, y : int) -> int {
   return x + y  
    
  }
  b = 2
  result = add(b + 2, 2)
  extra = 5
  finalResult = result + extra
  print(finalResult)
  """

  val program3 =
    """
def swap(t: tuple[int, int]) -> tuple[int, int] {
  return {t._1, t._0}
}

s = swap({5, 9})
print(s._0)
print(s._1)
  """

  Program(
    HashMap(
      swap_start -> List(
        MovQ(Rdi, Rbp),
        MovQ(Immediate(24), Rdi),
        CallQ(allocate, 1),
        MovQ(Rax, Rcx),
        MovQ(Immediate(2), Deref(Rcx, 0)),
        MovQ(Deref(Rbp, 16), Rdx),
        MovQ(Deref(Rbp, 8), Rsi),
        MovQ(Rdx, Deref(Rcx, 8)),
        MovQ(Rsi, Deref(Rcx, 16)),
        MovQ(Rcx, Rax),
        Jmp(swap_conclusion)
      ),
      swap_conclusion -> List(AddQ(Immediate(16), Rsp), PopQ(Rbp), RetQ),
      main_conclusion -> List(AddQ(Immediate(16), Rsp), PopQ(Rbp), RetQ),
      main -> List(
        PushQ(Rbp),
        MovQ(Rsp, Rbp),
        SubQ(Immediate(16), Rsp),
        CallQ(initialize, 0),
        Jmp(main_start)
      ),
      swap -> List(
        PushQ(Rbp),
        MovQ(Rsp, Rbp),
        SubQ(Immediate(16), Rsp),
        Jmp(swap_start)
      ),
      main_start -> List(
        MovQ(Immediate(24), Rdi),
        CallQ(allocate, 1),
        MovQ(Rax, Rcx),
        MovQ(Immediate(2), Deref(Rcx, 0)),
        MovQ(Immediate(5), Deref(Rcx, 8)),
        MovQ(Immediate(9), Deref(Rcx, 16)),
        MovQ(Rcx, Rdi),
        CallQ(swap, 1),
        MovQ(Rax, Rbx),
        MovQ(Deref(Rbx, 8), Rcx),
        MovQ(Rcx, Rdi),
        CallQ(print_int, 1),
        MovQ(Deref(Rbx, 16), Rcx),
        MovQ(Rcx, Rdi),
        CallQ(print_int, 1),
        MovQ(Immediate(0), Rax),
        Jmp(main_conclusion)
      )
    )
  )

  val program2 =
    """
    def add(x: int, y: int) -> int {
      return x + y
    }

    def do(x: int) -> int{ 
      return add(x, 2)
    
    }

    print(do(5))
    """

  val parsed = parse(program3)
  println("parsed: " + parsed)
  val sexped = LCoreReader.fromSExpToModule(parsed)
  println("sexped: " + sexped)
  val shrinked = shrink(sexped)
  println("shrinked: " + shrinked)
  val removed = removeComplexOperands(shrinked)
  println("removed: " + removed)
  val controlled = explicateControl(removed)
  println("controlled: " + controlled)
  val selected = selectInstructions(controlled)
  println("selected: " + selected)
  val regProg = allocateRegisters(selected)
  println("after register allocation: " + regProg)
  val patched = patchProgWithFun(regProg)
  println("patched: " + patched)
  val noOpsEliminated = eliminateNoOps(patched)
  println("noOpsEliminated: " + noOpsEliminated)
  val finalProg = generatePreludeAndConclusion(noOpsEliminated)
  println("finalProg: " + finalProg)

def compile(input: Path): Path = {
  val basename = input.getFileName.toString.replace(".lang", "")
  val source = readFile(input)
  val sexped = parse(source)
  val target = transformTox86(sexped)
  assemble(target, basename)
}

def transformTox86(prog: SExp): Program = {
  val parsed = LCoreReader.fromSExpToModule(prog)
  val shrinked = shrink(parsed)
  val withOutComplexOperands = removeComplexOperands(shrinked)
  val explicatedControl = explicateControl(withOutComplexOperands)
  val instrsWithVar = selectInstructions(explicatedControl)
  val progsWithReg = allocateRegisters(instrsWithVar)
  val patchedInstrs = patchProgWithFun(progsWithReg)
  val noOps = eliminateNoOps(patchedInstrs)
  val finalProg = generatePreludeAndConclusion(noOps)
  finalProg

}

def readFile(path: Path): String = {
  val source = scala.io.Source.fromFile(path.toFile)
  try source.getLines.mkString("\n")
  finally source.close()
}
