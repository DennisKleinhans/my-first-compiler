package analysis

import scala.collection.mutable

case object Top
case object Bottom

type Top = Top.type
type Bottom = Bottom.type

object analyzed {

  /**
   * An analyzed basic block
   *
   * @tparam A analysis result
   * @tparam I instruction
   *
   * @param result final result of performing the analysis on this block
   * @param instructions list of instructions paired with the result _before_ analysing this instruction
   */
  case class Block[A, I](result: A, instructions: List[(I, A)])

  /**
   * An analyzed program
   *
   * @tparam A analysis result
   * @tparam I instruction
   *
   * @param blocks mapping from label to analyzed [[Block]]
   */
  case class Program[A, I](blocks: Map[String, Block[A, I]])
}


/**
 * Performs a forward data-flow analysis on the given list of instructions,
 * starting with [[before]] and running [[analyse]] on each instruction (of type [[I]]).
 *
 * This analysis starts with the _first_ instruction in the list.
 * - [[analyzed.Block.result]]: contains the analysis _after_ the last instruction
 * - [[analyzed.Block.instructions]]: each pair [[(I, A)]] contains the analysis _before_ the instruction
 */
def forwards[A, I](instructions: List[I], before: A)(analyse: (I, A) => A): analyzed.Block[A, I] =
  var current = before
  val result = instructions.map { instr =>
    val before = current
    current = analyse(instr, before)
    (instr, before)
  }
  analyzed.Block(current, result)


/**
 * Performs a backward data-flow analysis on the given list of instructions,
 * starting with [[after]] as last analysis and running [[analyse]] in reverse on each instruction (of type [[I]]).
 *
 * This analysis starts with the _last_ instruction in the list.
 * - [[analyzed.Block.result]]: contains the analysis _before_ the first instruction
 * - [[analyzed.Block.instructions]]: each pair [[(I, A)]] contains the analysis _after_ the instruction
 */
def backwards[A, I](instructions: List[I], after: A)(analyse: (I, A) => A): analyzed.Block[A, I] =
  val analyzed.Block(result, instrs) = forwards(instructions.reverse, after)(analyse)
  analyzed.Block(result, instrs.reverse)


/**
 * Performs a forward data-flow analysis over the program defined by [[blocks]]
 * and the control-flow [[graph]] between blocks.
 *
 * The analysis starts at the entry points in [[extrema]], using [[iota]] as the
 * initial input analysis value for these blocks.
 *
 * The analysis propagates values of type [[A]] through the graph, using the
 * provided [[join]] operation to merge incoming flows and [[bottom]] as the
 * default value for uninitialized inputs.
 *
 * The transfer function [[analyse]] is applied to each instruction [[I]] with the
 * current input value to compute the output.
 */
def forwards[A, I](
  blocks: Map[String, List[I]],
  graph: Graph[String],
  bottom: A,
  join: (A, A) => A,
  iota: A,
  extrema: Set[String]
)(analyse: (I, A) => A): analyzed.Program[A, I] =
  analyze(false, blocks, graph, bottom, join, iota, extrema)(analyse)


/**
 * Performs a backward data-flow analysis over the program defined by [[blocks]]
 * and the control-flow [[graph]] between blocks.
 *
 * The analysis starts at the exit points in [[extrema]], using [[iota]] as the
 * initial output analysis value for these blocks.
 *
 * The analysis propagates values of type [[A]] in reverse through the graph, using
 * the provided [[join]] operation to merge incoming flows and [[bottom]] as the
 * default value for uninitialized outputs.
 *
 * The transfer function [[analyse]] is applied to each instruction [[I]] with the
 * current output value to compute the required input.
 */
def backwards[A, I](
  blocks: Map[String, List[I]],
  graph: Graph[String],
  bottom: A,
  join: (A, A) => A,
  iota: A,
  extrema: Set[String]
)(analyse: (I, A) => A): analyzed.Program[A, I] =
  analyze(true, blocks, graph, bottom, join, iota, extrema)(analyse)


private def analyze[A, I](
  reverse: Boolean,
  blocks: Map[String, List[I]],
  graph: Graph[String],
  bottom: A,
  join: (A, A) => A,
  // this could also be Map[String, A] in the future
  iota: A,
  extrema: Set[String]
)(analyse: (I, A) => A): analyzed.Program[A, I] =

  val original = if reverse then graph.transpose else graph
  val transposed = if reverse then graph else graph.transpose

  def analyseBlock(instrs: List[I], init: A) =
    if reverse
    then backwards(instrs, init)(analyse)
    else forwards(instrs, init)(analyse)

  val after = analyzeDataflow(original, bottom, join, iota, extrema) {
    case (label, after) => analyseBlock(blocks.getOrElse(label, Nil), after).result
  }

  analyzed.Program(
    blocks.map {
      case (label, instrs) if extrema contains label =>
        label -> analyseBlock(instrs, iota)
      case (label, instrs) =>
        val predecessors = transposed.neighbours.getOrElse(label, Set.empty)
        val incomingStates = predecessors.map(pred => after.getOrElse(pred, bottom))
        label -> analyseBlock(instrs, incomingStates.foldLeft(bottom)(join))
    }
  )


/**
 * Performs a dataflow analysis over a directed [[graph]] by applying the [[transfer]]
 * function and [[join]]ing results.
 *
 * Returns Map[V, R], mapping each vertex [[V]] to the respective result of the analysis [[R]].
 */
def analyzeDataflow[V, R](
    graph: Graph[V],
    bottom: R,
    join: (R, R) => R,
    iota: R,
    extrema: Set[V]
)(transfer: (V, R) => R): Map[V, R] =
  val transposed = graph.transpose

  // the "output" so far
  val mapping: mutable.Map[V, R] = mutable.Map.from(graph.vertices.map { v => v -> bottom })
  val worklist = mutable.ArrayDeque.from(extrema ++ graph.vertices)
  while (worklist.nonEmpty) {
    val v: V = worklist.removeLast()
    val input: R =
      if extrema contains v then
        iota
      else {
        val inputs: Set[R] = transposed.neighbours.getOrElse(v, Set.empty).map { w => mapping(w) }
        inputs.fold(bottom)(join)
      }
    val output: R = transfer(v, input)
    if output != mapping(v) then
      mapping(v) = output
      val vs = graph.neighbours.getOrElse(v, Set.empty)
      worklist.prependAll(vs)
  }
  mapping.toMap


/**
 * Generates a visual representation of the analyzed program [[p]] and basic block graph [[graph]].
 *
 * Uses [[showA]] to display analysis results and [[showI]] to display individual instructions.
 *
 * Saves the rendered graph to a file [[out]] defaulting to `graph.png`.
 */
def dumpAnalysis[A, I](
  p: analyzed.Program[A, I],
  graph: Graph[String],
  showA: A => String,
  showI: I => String,
  out: String = "graph.png"
): Unit =
  def renderBlock(block: String): String =
    if block == "conclusion" then "conclusion"
    else p.blocks(block) match {
      case analyzed.Block(result, instrs) =>
        val instructions = instrs.map { case (i, annot) =>
          val formatted = showI(i)
          val padding = " " * (16 - formatted.length)
          s"""  ${showI(i)}$padding<FONT COLOR="gray">${showA(annot)}</FONT>"""
        }.mkString("<BR ALIGN=\"LEFT\"/>")
        val padding = " " * (16 - block.length)
        s"""<
          |<FONT FACE="monospace">
          |$block: $padding<FONT COLOR="gray">${showA(result)}</FONT><BR ALIGN="LEFT"/>
          |$instructions<BR ALIGN="LEFT"/>
          |</FONT>
          |>""".stripMargin
    }

  generateGraph(graphviz[String](graph, renderBlock,
    ids = v => v,
    directed = true,
    fontname = "monospace",
    shape = graphviz.Shape.Rectangle,
    htmlLabels = true
  ), out)
