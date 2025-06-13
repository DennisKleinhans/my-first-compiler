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

}
