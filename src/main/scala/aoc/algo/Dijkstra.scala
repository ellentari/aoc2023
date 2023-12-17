package aoc.algo

import scala.collection.mutable
import scala.math.Numeric.Implicits.infixNumericOps

object Dijkstra {

  def findMinPathCost[A, C: Ordering : Numeric](start: A*)(
    next: A => List[(A, C)],
    isEnd: A => Boolean
  ): Option[C] = findMinPath(start :_*)(next, isEnd).map(_._1)

  def findMinPath[A, C: Ordering : Numeric](start: A*)(
    next: A => List[(A, C)],
    isEnd: A => Boolean
  ): Option[(C, List[A])] =
    findMaxPath(start :_*)(next, isEnd)(implicitly, Ordering[C].reverse)

  def findMaxPathCost[A, C: Ordering : Numeric](start: A*)(
    next: A => List[(A, C)],
    isEnd: A => Boolean
  ): Option[C] = findMaxPath(start :_*)(next, isEnd).map(_._1)

  def findMaxPath[A, C: Numeric](start: A*)(
    next: A => List[(A, C)],
    isEnd: A => Boolean
  )(implicit ord: Ordering[C]): Option[(C, List[A])] = {

    val priorityQueue = mutable.PriorityQueue.empty[(A, C)](Ordering.by[(A, C), C](_._2)(ord))
    val totalCost = mutable.HashMap.empty[A, C]
    val parent = mutable.HashMap.empty[A, A]
    val done = mutable.HashSet.empty[A]

    start.foreach(totalCost.update(_, Numeric[C].zero))
    start.foreach(s => priorityQueue.enqueue((s, Numeric[C].zero)))

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
                totalCost.get(b).forall(ord.lt(_, costA + costB))
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

  def findMaxPathCostsToAll[A, C: Numeric](start: A)(
    next: A => List[(A, C)]
  )(implicit ord: Ordering[C]): Map[A, C] = {

    val priorityQueue = mutable.PriorityQueue.empty[(A, C)](Ordering.by[(A, C), C](_._2)(ord))
    val totalCost = mutable.HashMap.empty[A, C]
    val parent = mutable.HashMap.empty[A, A]
    val done = mutable.HashSet.empty[A]

    totalCost.update(start, Numeric[C].zero)
    priorityQueue.enqueue((start, Numeric[C].zero))

    while (priorityQueue.nonEmpty) {
      val (a, _) = priorityQueue.dequeue()

      if (!done(a)) {

        val costA = totalCost(a)

        next(a)
          .filter { case (b, costB) =>
            !done.contains(b) &&
              totalCost.get(b).forall(ord.lt(_, costA + costB))
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

    totalCost.toMap
  }

}
