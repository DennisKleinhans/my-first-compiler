package compiler

import analysis.*
import analysis.analyzed
import analysis.Graph
import analysis.dsatur
import x86.Reg.*
import x86.{Instr => x86Instr}
import compiler.x86VarIf.Instr
import compiler.CommonNodes.Atom
import compiler.x86VarIf.Immediate

object RegisterAllocation {

  val registerColors: Coloring[x86VarIf.Location] = Map(
    R15 -> -5,
    R11 -> -4,
    Rbp -> -3,
    Rsp -> -2,
    Rax -> -1,
    Rcx -> 0,
    Rdx -> 1,
    Rsi -> 2,
    Rdi -> 3,
    R8 -> 4,
    R9 -> 5,
    R10 -> 6,
    Rbx -> 7,
    R12 -> 8,
    R13 -> 9,
    R14 -> 10
  )

  /** Builds a basic block graph from the given x86VarIf.Program. Each block is
    * represented by its label, and edges are created based on the control flow
    * between blocks.
    *
    * @param program
    *   The x86VarIf.Program to analyze
    * @return
    *   A Graph where vertices are block labels and edges represent control flow
    *   between blocks (i.e., jumps and fall-throughs).
    */
  def basicblockGraph(program: x86VarIf.Program): Graph[String] = {
    // Collect all block labels in order (preserving insertion order if possible)
    val labels = program.blocks.keys.toList

    // Initialize graph with all vertices
    var g: Graph[String] = Graph.empty
    labels.foreach(label => g = g ++ Graph.vertex(label))

    // For each block, determine successors and add edges
    for ((label, idx) <- labels.zipWithIndex) {
      val instrs = program.blocks(label)
      // Identify fall-through successor (next block in list)
      val fallThrough =
        if (idx + 1 < labels.length) Some(labels(idx + 1)) else None

      // Collect jump targets from instructions
      // (both unconditional and conditional jumps)
      val jumpTargets = instrs.collect {
        case Instr.Jmp(target)      => target
        case Instr.JmpIf(_, target) => target
      }.toSet

      // If the last instruction is a jump, it does not fall through to the next block, so we do not include the fall-through in the successors if the last instruction is a jump.
      val endsWithJump = instrs.lastOption.exists {
        case Instr.Jmp(_) | Instr.JmpIf(_, _) => true
        case _                                => false
      }

      // If the last instruction is not a jump, we include the fall-through in the successors
      val fallThroughSet =
        if (!endsWithJump)
          fallThrough.toSet
        else
          Set.empty

      val successors = jumpTargets ++ fallThroughSet

      successors.foreach { succ =>
        g = g ++ Graph.directedEdge(label, succ)
      }

    }

    g
  }

  /** Reads an argument and returns the set of locations it reads.
    *
    * @param arg
    *   The argument to read, which can be a location or an immediate value.
    * @return
    *   A set of locations that the argument reads. If the argument is an
    *   immediate, it returns an empty set, as immediate values do not read any
    *   locations.
    */
  def readArg(arg: x86VarIf.Arg): Set[x86VarIf.Location] = arg match {
    case loc: x86VarIf.Location  => Set(loc)
    case imm: x86VarIf.Immediate => Set.empty
  }

  /** Reads the locations accessed by the given instruction.
    *
    * @param instr
    *   The instruction to analyze, which can be any x86VarIf.Instr.
    * @return
    *   A set of locations that the instruction reads.
    */
  def read(instr: x86VarIf.Instr): Set[x86VarIf.Location] = instr match
    case Instr.MovQ(src, dest) => readArg(src)
    case Instr.AddQ(src, dest) => readArg(src) ++ readArg(dest)
    case Instr.SubQ(src, dest) => readArg(src) ++ readArg(dest)
    case Instr.NegQ(arg)       => readArg(arg)
    case Instr.CallQ("print_int", arity) =>
      Set(Rdi) // only Rdi is used for print
    case Instr.PushQ(arg)          => readArg(arg)
    case Instr.PopQ(arg)           => Set.empty
    case Instr.RetQ                => Set(Rax) // Ret reads Rax
    case Instr.CmpQ(lower, higher) => readArg(lower) ++ readArg(higher)
    case Instr.MovZBQ(src, dest)   => readArg(src)
    case Instr.Jmp(label)          => Set.empty
    case Instr.JmpIf(cc, label)    => Set.empty
    case Instr.Set(cc, dest)       => Set.empty
    case _                         => Set.empty

  /** Returns the set of locations that are written by the given instruction.
    *
    * @param instr
    *   The instruction to analyze, which can be any x86VarIf.Instr.
    * @return
    *   A set of locations that the instruction writes to.
    */
  def written(instr: x86VarIf.Instr): Set[x86VarIf.Location] = instr match
    case Instr.MovQ(src, dest) => Set(dest)
    case Instr.AddQ(src, dest) => Set(dest)
    case Instr.SubQ(src, dest) => Set(dest)
    case Instr.NegQ(arg)       => Set(arg)
    case Instr.CallQ(lable, arity) =>
      Set(Rax, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11)
    case Instr.PushQ(arg)          => Set(Rsp)
    case Instr.PopQ(arg)           => readArg(arg) ++ Set(Rsp)
    case Instr.RetQ                => Set.empty
    case Instr.CmpQ(lower, higher) => Set.empty
    case Instr.MovZBQ(src, dest)   => Set(dest)
    case Instr.Jmp(label)          => Set.empty
    case Instr.JmpIf(cc, label)    => Set.empty
    case Instr.Set(cc, dest)       => Set(dest)
    case _                         => Set.empty

  /** Performs a backward data-flow analysis to uncover live variables in the
    * given x86VarIf.Program.
    *
    * @param program
    *   The x86VarIf.Program to analyze, which contains a set of blocks with
    *   instructions.
    * @return
    *   An analyzed.Program where each instruction is annotated with a set of
    *   live locations after the instruction.
    */
  def uncoverLive(
      program: x86VarIf.Program
  ): analyzed.Program[Set[x86VarIf.Location], x86VarIf.Instr] = {

    backwards[Set[x86VarIf.Location], x86VarIf.Instr](
      blocks = program.blocks,
      graph = basicblockGraph(program),
      bottom = Set.empty[x86VarIf.Location],
      join = _ ++ _,
      iota = Set(Rax, Rsp),
      extrema = Set("conclusion")
    ) { (instr, liveAfter) =>
      (liveAfter -- written(instr)) ++ read(instr)
    }
  }

  /** Generates an interference graph for the given x86VarIf.Program. This graph
    * represents the interference between variables based on their live ranges
    * and definitions.
    *
    * @param prog
    *   The x86VarIf.Program to analyze, which contains a set of blocks with
    *   instructions and their live ranges.
    * @return
    *   A Graph where vertices are x86VarIf.Location and edges represent
    *   interference between them (i.e., if two locations are live at the same
    *   time, they are connected by an edge).
    */
  def interferenceGraph(
      prog: analyzed.Program[Set[x86VarIf.Location], x86VarIf.Instr]
  ): Graph[x86VarIf.Location] =
    var g: Graph[x86VarIf.Location] = Graph.empty
    prog.blocks.foreach { case (_, analyzed.Block(_, instrs)) =>
      g = g ++ interferenceGraph(instrs)
    }

    g

  /** Generates an interference graph from a list of instructions and their live
    * ranges. Each instruction is paired with a set of locations that are live
    * after the instruction.
    *
    * @param instrs
    *   List of tuples where each tuple contains an instruction and a set of
    *   live locations after that instruction.
    * @return
    *   A Graph where vertices are x86VarIf.Location and edges represent
    *   interference between them (i.e., if two locations are live at the same
    *   time, they are connected by an edge).
    */
  def interferenceGraph(
      instrs: List[(Instr, Set[x86VarIf.Location])]
  ): Graph[x86VarIf.Location] =

    def isTemp(loc: x86VarIf.Location): Boolean = loc match {
      case _: x86VarIf.Variable => true
      case _                    => false
    }

    var g = Graph.empty[x86VarIf.Location]
    for ((instr, liveAfter) <- instrs) {
      val defs = written(instr).filter(isTemp)
      for {
        d <- defs
        v <- liveAfter.filter(isTemp) if d != v
      }
        g = g ++ Graph.edge(d, v)
    }
    g

}
