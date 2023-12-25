package aoc.algo

import scala.collection.mutable
import scala.util.Random

object KargersAlgorithm {

  def minCuts(vertexCount: Int, edges: Vector[(Int, Int)]): List[(Int, Int)] = {
    val ds = new DisjointSet(vertexCount)

    var v = vertexCount
    while (v > 2) {
      val i = Random.nextInt(edges.size)

      val s1 = ds.find(edges(i)._1)
      val s2 = ds.find(edges(i)._2)

      if (s1 != s2) {
        ds.union(s1, s2)
        v -= 1
      }
    }

    val cuts = mutable.ListBuffer.empty[(Int, Int)]

    for (edge <- edges) {
      val s1 = ds.find(edge._1)
      val s2 = ds.find(edge._2)

      if (s1 != s2)
        cuts.addOne(edge)
    }

    cuts.toList
  }

}
