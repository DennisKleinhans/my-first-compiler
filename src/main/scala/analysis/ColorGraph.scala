package analysis

import scala.annotation.tailrec

// Coloring
// --------

type Color = Int

type Coloring[V] = Map[V, Color]
type Saturation = Set[Color]

def saturation[V](loc: V, graph: Graph[V], coloring: Coloring[V]): Saturation =
  graph.neighbours(loc).flatMap(coloring.get)

// Note: This has a horrible time complexity!
def maximalSaturation[V](graph: Graph[V], coloring: Coloring[V]): Option[(V, Saturation)] =
  graph.vertices
    .filterNot { v => coloring.isDefinedAt(v) }
    .map { v => v -> saturation(v, graph, coloring) }
    .maxByOption { case (v, sat) => sat.size }

def nextColor(saturation: Saturation): Color =
  var i = 0
  while (saturation contains i) { i = i + 1 }
  i


def dsatur[V](graph: Graph[V], initColors: Coloring[V]): Coloring[V] =
  var coloring: Coloring[V] = Map.empty[V,Color]

  @tailrec
  def go(): Unit =
    maximalSaturation(graph, initColors ++ coloring) match {
      case Some((u, sat)) =>
        coloring = coloring.updated(u, nextColor(sat))
        go()
      case None =>
    }
  go()
  coloring

