package aoc.util

case class Interval(start: Long, end: Long) {

  require(start <= end, s"Start must be <= end, but $start > $end")

  def contains(other: Interval): Boolean =
    start <= other.start && other.end <= end

  def intersects(other: Interval): Boolean =
    other.start <= end && start <= other.end

  def intersect(other: Interval): Option[Interval] =
    Option.when(intersects(other))(Interval(start max other.start, end min other.end))

  def union(other: Interval): Option[Interval] =
    Option.when(intersects(other))(Interval(start min other.start, end max other.end))

  def remove(interval: Interval): List[Interval] =
    intersect(interval) match {
      case None => List(this)
      case Some(intersection) =>
        val before = Interval.safe(start min interval.start, intersection.start - 1)
        val after = Interval.safe(intersection.end + 1, end max interval.end)

        before.toList ++ after
    }

  def removeAll(intervals: List[Interval]): List[Interval] =
    intervals.foldLeft(List(this))((remaining, toRemove) => remaining.flatMap(_.remove(toRemove)))

  def length: Long = end - start + 1

  override def toString: String = s"[$start, $end]"

}

object Interval {

  def safe(start: Long, end: Long): Option[Interval] =
    Option.when(start <= end)(Interval(start, end))

  def ofLength(start: Long, length: Long): Interval =
    Interval(start, start + length - 1)

}
