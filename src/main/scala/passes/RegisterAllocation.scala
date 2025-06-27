package passes

import analysis.*
import analysis.analyzed
import analysis.Graph
import analysis.dsatur
import x86.Reg.*
import x86.{Instr => x86Instr}
import compiler.{x86Var, CommonNodes}
import CommonNodes.Atom
import x86Var.Instr
import x86Var.Immediate

object RegisterAllocation {

  val registerColors: Coloring[x86Var.Location] = Map(
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

  val callerSavedRegistersColors: Coloring[x86Var.Location] = Map(
    Rax -> -1,
    Rcx -> 0,
    Rdx -> 1,
    Rsi -> 2,
    Rdi -> 3,
    R8 -> 4,
    R9 -> 5,
    R10 -> 6,
    R11 -> -4
  )

  val callerSavedRegisters = Set(Rax, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11)
  val calleeSavedRegisters = Set(Rsp, Rbp, Rbx, R12, R13, R15, R15)

  /** Builds a basic block graph from the given x86Var.Program. Each block is
    * represented by its label, and edges are created based on the control flow
    * between blocks.
    *
    * @param program
    *   The x86Var.Program to analyze
    * @return
    *   A Graph where vertices are block labels and edges represent control flow
    *   between blocks (i.e., jumps and fall-throughs).
    */
  def basicblockGraph(program: x86Var.Program): Graph[String] = {
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
  def readArg(arg: x86Var.Arg): Set[x86Var.Location] = arg match {
    case loc: x86Var.Location  => Set(loc)
    case imm: x86Var.Immediate => Set.empty
  }

  /** Reads the locations accessed by the given instruction.
    *
    * @param instr
    *   The instruction to analyze, which can be any x86Var.Instr.
    * @return
    *   A set of locations that the instruction reads.
    */
  def read(instr: x86Var.Instr): Set[x86Var.Location] = instr match
    case Instr.MovQ(src, dest) => readArg(src)
    case Instr.AddQ(src, dest) => readArg(src) ++ readArg(dest)
    case Instr.SubQ(src, dest) => readArg(src) ++ readArg(dest)
    case Instr.NegQ(arg)       => readArg(arg)
    case Instr.CallQ("print_int", arity) =>
      Set(
        Rdi // only Rdi is used for as argument register for print
      ) ++ calleeSavedRegisters.asInstanceOf[Set[x86Var.Location]]
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
    *   The instruction to analyze, which can be any x86Var.Instr.
    * @return
    *   A set of locations that the instruction writes to.
    */
  def written(instr: x86Var.Instr): Set[x86Var.Location] = instr match
    case Instr.MovQ(src, dest) => Set(dest)
    case Instr.AddQ(src, dest) => Set(dest)
    case Instr.SubQ(src, dest) => Set(dest)
    case Instr.NegQ(arg)       => Set(arg)
    case Instr.CallQ(lable, arity) =>
      callerSavedRegisters.asInstanceOf[Set[x86Var.Location]]
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
    * given x86Var.Program.
    *
    * @param program
    *   The x86Var.Program to analyze, which contains a set of blocks with
    *   instructions.
    * @return
    *   An analyzed.Program where each instruction is annotated with a set of
    *   live locations after the instruction.
    */
  def uncoverLive(
      program: x86Var.Program
  ): analyzed.Program[Set[x86Var.Location], x86Var.Instr] = {

    backwards[Set[x86Var.Location], x86Var.Instr](
      blocks = program.blocks,
      graph = basicblockGraph(program),
      bottom = Set.empty[x86Var.Location],
      join = _ ++ _,
      iota = Set(Rax, Rsp),
      extrema = Set("conclusion")
    ) { (instr, liveAfter) =>
      (liveAfter -- written(instr)) ++ read(instr)
    }
  }

  def isTemp(loc: x86Var.Location): Boolean = loc match {
    case _: x86Var.Variable => true
    case _                  => false
  }

  /** Generates an interference graph for the given x86Var.Program. This graph
    * represents the interference between variables based on their live ranges
    * and definitions.
    *
    * @param prog
    *   The x86Var.Program to analyze, which contains a set of blocks with
    *   instructions and their live ranges.
    * @return
    *   A Graph where vertices are x86Var.Location and edges represent
    *   interference between them (i.e., if two locations are live at the same
    *   time, they are connected by an edge).
    */
  def interferenceGraph(
      prog: analyzed.Program[Set[x86Var.Location], x86Var.Instr]
  ): Graph[x86Var.Location] =
    // start with an empty graph and add edges from each block's subgraph
    var g: Graph[x86Var.Location] = Graph.empty

    // add all caller saved registers to the interfence graph at the beginning
    callerSavedRegisters.foreach(r => g = g ++ Graph.vertex(r))

    prog.blocks.foreach { case (_, analyzed.Block(_, instrs)) =>
      g = g ++ interferenceGraph(instrs)
    }

    // collect all variables that are appear either in liveAfter sets or written sets
    val allTemps = prog.blocks.values.flatMap { block =>
      block.instructions.flatMap { case (instr, liveAfter) =>
        (liveAfter ++ written(instr)).filter(isTemp)
      }
    }.toSet

    // add each variable as a vertex in the graph so isolated variables are also included
    // this is important for the coloring algorithm to work correctly
    // otherwise, isolated variables would not be included in the graph
    allTemps.foreach { loc =>
      g = g ++ Graph.vertex(loc)
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
    *   A Graph where vertices are x86Var.Location and edges represent
    *   interference between them (i.e., if two locations are live at the same
    *   time, they are connected by an edge).
    */
  def interferenceGraph(
      instrs: List[(Instr, Set[x86Var.Location])]
  ): Graph[x86Var.Location] =

    var g = Graph.empty[x86Var.Location]
    for ((instr, liveAfter) <- instrs) {
      instr match {
        // caller saved registers should interfere with all live variables at every call instruction
        case Instr.CallQ(_, _) =>
          for {
            v <- liveAfter.filter(isTemp)
            r <- callerSavedRegisters
          } {
            g = g ++ Graph.edge(v, r)
          }
        case _ =>
      }

      // only consider variables written by this instruction
      val defs = written(instr).filter(isTemp)
      for {
        d <- defs
        v <- liveAfter.filter(isTemp) if d != v
      }
        // add interference edge: d and v cannot share a register
        g = g ++ Graph.edge(d, v)
    }
    g

  /** Assigns homes to variables based on their coloring.
    *
    * @param coloring
    *   A mapping from x86Var.Location to Color, where each location is assigned
    *   a color representing its register or spill location.
    * @return
    *   A mapping from x86Var.Variable to x86Var.Location, where each variable
    *   is assigned a home location (either a physical register or a spill
    *   location).
    */
  def homesFor(
      coloring: Map[x86Var.Location, Color]
  ): Map[x86Var.Variable, x86Var.Location] = {
    // Inverted mapping: ColorID -> physical register
    val colorToReg: Map[Color, x86Var.Location] =
      registerColors.map { case (reg, col) => (col, reg) }

    // Fold over variables to assign homes
    coloring.keys
      .collect { case v: x86Var.Variable => v }
      .foldLeft((0, Map.empty[x86Var.Variable, x86Var.Location])) {
        case ((spillIdx, acc), v) =>
          coloring(v) match {
            case col if colorToReg.contains(col) =>
              (spillIdx, acc + (v -> colorToReg(col)))
            case _ =>
              val offset = -(spillIdx + 1) * 8
              val home = x86.Deref(x86.Reg.Rbp, offset)
              (spillIdx + 1, acc + (v -> home))
          }
      }
      ._2
  }

  /** Assigns homes to variables in the given x86Var.Program based on the
    * provided mapping of variables to locations. It rewrites the instructions
    * to use the assigned homes, replacing variables with their corresponding
    * locations (either registers or spill locations).
    *
    * @param program
    *   The x86Var.Program to rewrite, which contains a set of blocks with
    *   instructions.
    * @param homes
    *   A mapping from x86Var.Variable to x86Var.Location, where each variable
    *   is assigned a home location (either a physical register or a spill
    *   location).
    * @return
    *   A tuple containing the rewritten x86.Program and the maximum spill
    *   offset used in the program.
    */
  def assignHomes(
      program: x86Var.Program,
      homes: Map[x86Var.Variable, x86Var.Location]
  ): (x86.Program, Long) = {
    var maxSpillOffset = 0L

    // Rewrite arguments in instructions to use the assigned homes
    // and spill locations, while also tracking the maximum spill offset used.
    def rewriteArg(arg: x86Var.Arg): x86.Arg = arg match {
      case x86Var.Immediate(n)  => x86.Immediate(n)
      case reg: x86.Reg         => reg
      case byteReg: x86.ByteReg => byteReg

      case x86Var.Deref(loc, offset) =>
        rewriteLocation(loc) match
          case r: x86.Reg => x86.Deref(r, offset)
          case other      => sys error f"expected a Register, but got $other"

      case variable: x86Var.Variable =>
        homes.get(variable) match
          case Some(location) =>
            location match {
              case reg: x86.Reg => reg
              case spill @ x86.Deref(_, offset) =>
                maxSpillOffset = math.max(maxSpillOffset, -offset)
                spill
              case other =>
                sys.error(s"Unexpected location for variable $variable: $other")
            }

          case None =>
            sys.error(s"Variable $variable has no home assigned")

      case deref: x86.Deref => deref
      case global: x86.Global =>
        sys error s"Unexpected global: $global in assignHomes (rewriteArg)"

    }

    // Rewrite locations in instructions to use the assigned homes
    // and spill locations, while also tracking the maximum spill offset used.
    def rewriteLocation(loc: x86Var.Location): x86.Location = loc match {
      case x86Var.Deref(loc, offset) =>
        rewriteLocation(loc) match
          case r: x86.Reg => x86.Deref(r, offset)
          case other      => sys error f"expected a Register, but got $other"

      case variable: x86Var.Variable =>
        rewriteArg(variable).asInstanceOf[x86.Location]

      case reg: x86.Reg         => reg
      case byteReg: x86.ByteReg => byteReg
      case deref: x86.Deref     => deref
      case other                => sys.error(s"Unexpected location: $other")
    }

    // Rewrite each instruction in the program to use the assigned homes
    // and spill locations, while also wrapping CallQ instructions.
    // It returns a list of x86Instr, which are the rewritten instructions.
    def rewriteInstr(instr: x86Var.Instr): x86Instr = instr match {
      case x86Var.MovQ(src, dest) =>
        x86Instr.MovQ(rewriteArg(src), rewriteLocation(dest))
      case x86Var.AddQ(src, dest) =>
        x86Instr.AddQ(rewriteArg(src), rewriteLocation(dest))
      case x86Var.SubQ(src, dest) =>
        x86Instr.SubQ(rewriteArg(src), rewriteLocation(dest))
      case x86Var.NegQ(arg) =>
        x86Instr.NegQ(rewriteLocation(arg))
      case x86Var.CallQ(label, arity) =>
        x86Instr.CallQ(label, arity)
      case x86Var.PushQ(arg) =>
        x86Instr.PushQ(rewriteArg(arg))
      case x86Var.PopQ(arg) =>
        x86Instr.PopQ(rewriteArg(arg))
      case x86Var.RetQ => x86Instr.RetQ
      case x86Var.CmpQ(lower, higher) =>
        x86Instr.CmpQ(rewriteArg(lower), rewriteArg(higher))
      case x86Var.MovZBQ(src, dest) =>
        x86Instr.MovZBQ(rewriteArg(src), rewriteLocation(dest))
      case x86Var.Set(cc, dest) =>
        x86Instr.Set(cc, rewriteLocation(dest))
      case x86Var.Jmp(label)       => x86Instr.Jmp(label)
      case x86Var.JmpIf(cc, label) => x86Instr.JmpIf(cc, label)

      case _ => sys.error(s"Unsupported instruction: $instr")
    }

    val rewrittenBlocks = program.blocks.map { case (label, instrs) =>
      label -> instrs.map(rewriteInstr)
    }
    (x86.Program(rewrittenBlocks), maxSpillOffset)

  }

  /** Allocates registers for the given x86Var.Program using a graph-based
    * register allocation algorithm. It first uncovers live variables, builds an
    * interference graph, colors the graph using the DSATUR algorithm, and then
    * assigns homes to variables based on the coloring. The result is a tuple
    * containing the rewritten x86.Program with assigned homes and the maximum
    * spill offset used in the program.
    *
    * @param program
    *   The x86Var.Program to allocate registers for, which contains a set of
    *   blocks with instructions.
    * @return
    *   A tuple containing the rewritten x86.Program with assigned homes and the
    *   maximum spill offset used in the program.
    */
  def allocateRegisters(
      program: x86Var.Program
  ): (x86.Program, Long) = {
    val liveProg = uncoverLive(program)
    val graph = interferenceGraph(liveProg)
    val coloring = dsatur(graph, callerSavedRegistersColors)
    val homes = homesFor(coloring)
    assignHomes(program, homes)
  }
}
