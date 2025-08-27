package analysis

import scala.collection.mutable

case class Graph[V](neighbours: Map[V, Set[V]]) {

  lazy val vertices: Set[V] = neighbours.keySet ++ neighbours.values.flatten

  def ++(other: Graph[V]): Graph[V] =
    Graph(
      (neighbours.keys ++ other.neighbours.keys).map { key =>
        key -> (neighbours.getOrElse(key, Set.empty) ++ other.neighbours.getOrElse(key, Set.empty))
      }.toMap
    )

  def transpose: Graph[V] =
    var g = Map.empty[V, Set[V]]
    neighbours.keys.foreach { u => g = g + (u -> Set.empty[V]) }

    neighbours.foreach { case (u, outs) =>
      outs.foreach { v =>
        val current = g.getOrElse(v, Set.empty[V])
        g = g + (v -> (current + u))
      }
    }
    Graph(g)

  def reachable(n: V) : Set[V] =  {
      def dfs(current: V, visited: Set[V]): Set[V] = {
        if (visited.contains(current)) visited
        else {
          val neighbors = neighbours.getOrElse(current, Set.empty)
          neighbors.foldLeft(visited + current) { (acc, neighbor) =>
            dfs(neighbor, acc)
          }
        }
      }
      dfs(n,  Set.empty)
  }
}

object Graph {
  def empty[V]: Graph[V] = Graph(Map.empty)
  def vertex[V](loc: V): Graph[V] = Graph(Map(loc -> Set.empty))
  def edge[V](loc1: V, loc2: V): Graph[V] = Graph(Map(loc1 -> Set(loc2), loc2 -> Set(loc1)))
  def directedEdge[V](loc1: V, loc2: V): Graph[V] = Graph(Map(loc1 -> Set(loc2)))
}

object graphviz {
  enum Shape { case Circle, Rectangle }

  def apply[V](
    graph: Graph[V],
    vertexLabels : V => String,
    colors: Map[V, Int] = Map.empty[V, Int],
    palette: Vector[String] = Vector(
      "#ff9999", "#99ccff", "#99ff99", "#ffcc99",
      "#ccccff", "#ffff99", "#ffb3e6", "#c2f0c2"
    ),
    directed: Boolean = false,
    shape: graphviz.Shape = graphviz.Shape.Circle,
    fontname: String = "Times-Roman",
    htmlLabels: Boolean = false,
    ids : V => String = (v: V) => v.toString,
  ): String = {

    val edges = for {
      (fst, vs) <- graph.neighbours.toSet
      snd <- vs
    } yield (fst, snd)

    val vertices = graph.vertices

    // since colors can be negative...
    val offset = colors.values.minOption match {
      case Some(min) if min < 0 => min * -1
      case _ => 0
    }

    val nodeShape = shape match {
      case Shape.Circle => "ellipse"
      case Shape.Rectangle => "box"
    }

    val nodeStrings = vertices.map { v =>
      val label = vertexLabels(v)
      val quotedLabel = if htmlLabels && label.startsWith("<") then label else s""""$label""""
      colors.get(v) match {
        case Some(color) =>
          val hex = palette.lift(color + offset).getOrElse { sys error s"Not enough colors: ${color}" }
          s"""  ${ids(v)} [label=$quotedLabel, shape="$nodeShape", fontname="$fontname", style=filled, fillcolor="$hex"];"""
        case None =>
          s"""  ${ids(v)} [label=$quotedLabel, shape="$nodeShape", fontname="$fontname", style=filled, fillcolor="#ffffff"];"""
      }
    }

    val edgeStrings = if (directed) {
      // For directed graphs, show all edges as arrows
      edges.map { case (fst, snd) =>
        val a = ids(fst)
        val b = ids(snd)
        s"  \"$a\" -> \"$b\";"
      }
    } else {
      // For undirected graphs, deduplicate edges and use -- syntax
      edges.toSet.map { case (fst, snd) =>
        val a = ids(fst)
        val b = ids(snd)
        if (a <= b) s"  \"$a\" -- \"$b\";" else s"  \"$b\" -- \"$a\";"
      }
    }

    val graphType = if (directed) "digraph" else "graph"
    val body = (nodeStrings ++ edgeStrings).toList.sorted.mkString("\n")
    s"""
       |$graphType G {
       |$body
       |}
       |""".stripMargin.trim
  }
}

private def saveToFile(contents: String, filename: String): Unit =
  import java.nio.file.{Files, Paths}
  import java.nio.charset.StandardCharsets
  Files.write(Paths.get(filename), contents.getBytes(StandardCharsets.UTF_8))

def generateGraph(graphviz: String, out: String = "graph.png"): Unit =
  import sys.process._
  saveToFile(graphviz, "graph.dot")
  s"dot -Tpng graph.dot -o $out".!

def topologicalSort[V](graph: Graph[V]): List[V] = {
    val inDegree = mutable.Map[V, Int]().withDefaultValue(0)

    // Compute in-degree of each node
    for {
      (_, neighbors) <- graph.neighbours
      neighbor <- neighbors
    } inDegree(neighbor) += 1

    for (node <- graph.neighbours.keys if !inDegree.contains(node))
      inDegree(node) = 0

    // Initialize queue with nodes having in-degree 0
    val queue = mutable.Queue[V]()
    for ((node, degree) <- inDegree if degree == 0)
      queue.enqueue(node)

    val sorted = mutable.ListBuffer[V]()

    while (queue.nonEmpty) {
      val node = queue.dequeue()
      sorted += node

      for (neighbor <- graph.neighbours.getOrElse(node, Nil)) {
        inDegree(neighbor) -= 1
        if (inDegree(neighbor) == 0) {
          queue.enqueue(neighbor)
        }
      }
    }

    if (sorted.size != graph.neighbours.size) {
      throw new IllegalArgumentException("Graph has at least one cycle")
    }

    sorted.toList
  }


/**
 * Generates a visual representation of the program with [[blocks]] and basic block graph [[graph]].
 *
 * Uses [[showI]] to display individual instructions.
 *
 * Saves the rendered graph to a file [[out]] defaulting to `graph.png`.
 */
def dumpGraph[I](
  blocks: Map[String, List[I]],
  graph: Graph[String],
  showI: I => String,
  out: String = "graph.png"
): Unit =
  def renderBlock(block: String): String =
    if block == "conclusion" then "conclusion"
    else
      val instructions = blocks(block).map(i => s"  ${showI(i)}").mkString("<BR ALIGN=\"LEFT\"/>")
      s"""<
        |<FONT FACE="monospace">
        |$block:<BR ALIGN="LEFT"/>
        |$instructions<BR ALIGN="LEFT"/>
        |</FONT>
        |>""".stripMargin

  generateGraph(graphviz[String](graph, renderBlock,
    ids = v => v,
    directed = true,
    fontname = "monospace",
    shape = graphviz.Shape.Rectangle,
    htmlLabels = true
  ), out)
