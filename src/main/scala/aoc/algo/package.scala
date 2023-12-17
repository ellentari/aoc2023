package aoc

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

package object algo {

  def restorePath[A](a: A, parent: A => Option[A]): List[A] = {
    @tailrec
    def loop(a: A, acc: List[A]): List[A] =
      parent(a) match {
        case None => acc
        case Some(b) => loop(b, b :: acc)
      }

    loop(a, List(a))
  }


}
