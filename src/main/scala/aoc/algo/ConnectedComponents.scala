package aoc.algo

import scala.collection.mutable

object ConnectedComponents {

  def findConnectedComponents[A](vertices: List[A])(adjacencyList: Map[A, List[A]]): List[List[A]] = {
    val connectedComponents = mutable.HashMap.empty[A, Int]
    var cc = 0

    for (start <- vertices if !connectedComponents.contains(start)) {
      BFS.visitAll(start)(adjacencyList.getOrElse(_, Nil), a => connectedComponents.update(a, cc))
      cc += 1
    }

    connectedComponents
      .toList
      .groupMap(_._2)(_._1)
      .values
      .toList
  }

}
