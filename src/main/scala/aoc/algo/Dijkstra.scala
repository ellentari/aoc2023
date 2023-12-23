package aoc.algo

import scala.collection.mutable
import scala.math.Numeric.Implicits.infixNumericOps

object Dijkstra {

  def findMinPathCost[A](start: A*)(next: A => List[(A, Int)], isEnd: A => Boolean): Option[Int] =
    findMinPath(start: _*)(next, isEnd).map(_._1)

  def findMinPath[A](start: A*)(next: A => List[(A, Int)], isEnd: A => Boolean): Option[(Int, List[A])] = {

    val priorityQueue = mutable.PriorityQueue.empty[(A, Int)](Ordering.by[(A, Int), Int](-_._2))
    val totalCost = mutable.HashMap.empty[A, Int]
    val parent = mutable.HashMap.empty[A, A]
    val done = mutable.HashSet.empty[A]

    start.foreach(totalCost.update(_, 0))
    start.foreach(s => priorityQueue.enqueue((s, 0)))

    while (priorityQueue.nonEmpty) {
      val (a, _) = priorityQueue.dequeue()

      if (!done(a)) {

        val costA = totalCost(a)

        if (isEnd(a))
          return Some((costA, restorePath(a, parent.get)))
        else
          next(a)
            .filter { case (b, costB) =>
              !done.contains(b) &&
                totalCost.get(b).forall(_ > costA + costB)
            }
            .foreach { case (b, costB) =>
              val totalBCost = costA + costB

              parent.update(b, a)
              totalCost.update(b, totalBCost)
              priorityQueue.enqueue((b, totalBCost))
            }

        done.add(a)
      }
    }

    None
  }

}
