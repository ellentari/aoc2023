package aoc

import aoc.algo.{ConnectedComponents, KargersAlgorithm}

import scala.annotation.tailrec

object Day25 extends App {

  case class Graph[A](vertices: List[A], edges: Vector[(A, A)]) {
    def map[B](f: A => B): Graph[B] =
      Graph(vertices.map(f), edges.map { case (from, to) => (f(from), f(to)) })

    def removeEdges(toRemove: List[(A, A)]): Graph[A] =
      copy(edges = edges.filterNot(toRemove.contains))

    def adjacencyList: Map[A, List[A]] =
      edges
        .flatMap(edge => List(edge, edge.swap))
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.toList)
        .toMap

  }

  def solvePart1(graph: Graph[String]): Int = {
    val disconnected = disconnect(graph, maxEdgesToRemove = 3)

    val connectedComponents = ConnectedComponents.findConnectedComponents(graph.vertices)(disconnected.adjacencyList)

    assert(connectedComponents.size == 2)

    connectedComponents.map(_.size).product
  }

  private def disconnect(graph: Graph[String], maxEdgesToRemove: Int): Graph[String] = {
    val verticesMap = graph.vertices.zipWithIndex.toMap
    val mappedGraph = graph.map(verticesMap)

    @tailrec
    def loop(): List[(Int, Int)] = {
      val cuts = KargersAlgorithm.minCuts(mappedGraph.vertices.size, mappedGraph.edges)

      if (cuts.size <= maxEdgesToRemove) cuts
      else loop()
    }

    val cuts = loop()

    mappedGraph
      .removeEdges(cuts)
      .map(graph.vertices)
  }

  private def parseGraph(lines: List[String]): Graph[String] = {
    val edges = lines
      .flatMap {
        case s"$a: $bs" =>
          bs.split(" ").map(_.trim).toList
            .map { b => a -> b }
      }
      .toVector

    val vertices = edges.flatMap(e => List(e._1, e._2)).distinct.toList

    Graph(vertices, edges)
  }

  private val sample = parseGraph(
    """jqt: rhn xhk nvd
      |rsh: frs pzl lsr
      |xhk: hfx
      |cmg: qnr nvd lhk bvb
      |rhn: xhk bvb hfx
      |bvb: xhk hfx
      |pzl: lsr hfx nvd
      |qnr: nvd
      |ntq: jqt hfx bvb xhk
      |nvd: lhk
      |lsr: lhk
      |rzs: qnr cmg lsr rsh
      |frs: qnr lhk lsr"""
      .stripMargin
      .split("\n")
      .toList
  )

  private val input = parseGraph(Input.asList("day25.txt"))

  println(solvePart1(sample)) // 54
  println(solvePart1(input)) // 572000

}
