package aoc.algo

import scala.annotation.tailrec
import scala.collection.immutable.Queue

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

  def maxDistance[A](start: A)(next: A => List[A]): Int = {
    var maxDistance = 0
    visitAll(start)(next, (_, dist) => maxDistance = maxDistance max dist)
    maxDistance
  }

  def discoverRegion[A](start: A)(next: A => List[A]): List[A] =
    discoverRegion(List(start))(next)

  def discoverRegion[A](starts: List[A])(next: A => List[A]): List[A] = {
    val result = scala.collection.mutable.ListBuffer.empty[A]
    visitAll(starts)(next, (a, _) => result.append(a))
    result.toList
  }

  def visitAll[A](start: A)(next: A => List[A], onVisit: (A, Int) => Unit): Unit =
    visitAll(List(start))(next, onVisit)

  def visitAll[A](start: Iterable[A])(next: A => List[A], onVisit: (A, Int) => Unit): Unit = {
    @tailrec
    def loop(queue: Queue[(A, Int)], seen: Set[A]): Unit =
      queue.dequeueOption match {
        case None => ()
        case Some(((a, distance), tail)) =>
          onVisit(a, distance)

          val toVisit = next(a).filterNot(seen.contains)

          loop(tail ++ toVisit.map(n => n -> (distance + 1)), seen ++ toVisit)
      }

    loop(Queue.from(start.map(_ -> 0)), start.toSet)
  }


}
