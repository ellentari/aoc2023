package aoc.algo

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

object BFS {

  def shortestPathLength[A](start: A)(next: A => List[A], isEnd: A => Boolean): Option[Int] =
    shortestPath(start)(next, isEnd).map(_.length - 1)

  def shortestPath[A](start: A)(next: A => List[A], isEnd: A => Boolean): Option[List[A]] = {

    @tailrec
    def loop(queue: Queue[A], parent: Map[A, A], seen: Set[A]): Option[List[A]] =
      queue.dequeueOption match {
        case None => None
        case Some((a, tail)) =>

          if (isEnd(a)) Some(restorePath(a, parent.get))
          else {
            val toVisit = next(a).filterNot(seen.contains)

            loop(tail ++ toVisit, parent ++ toVisit.map(_ -> a), seen ++ toVisit)
          }
      }

    loop(Queue(start), Map.empty, Set(start))
  }

  def shortestPathLengthsToAll[A](start: A)(next: A => List[A], isEnd: A => Boolean): List[(A, Int)] = {
    val result = mutable.ListBuffer.empty[(A, Int)]

    visitAllWithDistance(List(start))(
      (a, _) => if (a == start || !isEnd(a)) next(a) else Nil,
      (a, distance) =>
        if (a != start && isEnd(a))
          result.append(a -> distance)
    )

    result.toList
  }

  def maxDistance[A](start: A)(next: A => List[A]): Int = {
    var maxDistance = 0
    visitAllWithDistance(List(start))((a, _) => next(a), (_, dist) => maxDistance = maxDistance max dist)
    maxDistance
  }

  def discoverRegion[A](start: A)(next: A => List[A]): List[A] =
    discoverRegion(List(start))(next)

  def discoverRegion[A](starts: List[A])(next: A => List[A]): List[A] = {
    val result = mutable.ListBuffer.empty[A]
    visitAll(starts)(next, a => result.append(a))
    result.toList
  }

  def visitAll[A](start: A)(next: A => List[A], onVisit: A => Unit): Unit =
    visitAll(List(start))(next, onVisit)

  def visitAll[A](start: Iterable[A])(next: A => List[A], onVisit: A => Unit): Unit =
    visitAllWithDistance(start)((a, _) => next(a), (a, _) => onVisit(a))

  def visitAllWithDistance[A](start: Iterable[A])(next: (A, Int) => List[A], onVisit: (A, Int) => Unit): Unit = {
    @tailrec
    def loop(queue: Queue[(A, Int)], seen: Set[A]): Unit =
      queue.dequeueOption match {
        case None => ()
        case Some(((a, distance), tail)) =>
          onVisit(a, distance)

          val toVisit = next(a, distance).filterNot(seen.contains)

          loop(tail ++ toVisit.map(n => n -> (distance + 1)), seen ++ toVisit)
      }

    loop(Queue.from(start.map(_ -> 0)), start.toSet)
  }


}
