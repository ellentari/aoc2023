package aoc

import scala.collection.mutable

object Day12 extends App {

  sealed trait Condition

  object Condition {
    case object Operational extends Condition
    case object Damaged extends Condition
    case object Unknown extends Condition
  }

  case class Record(arrangement: Vector[Condition], damagedGroups: Vector[Int]) {
    def unfold(copies: Int): Record = {
      val newGroups = (0 until copies).flatMap(_ => damagedGroups).toVector

      val newArrangement = mutable.ArrayBuffer.empty[Condition]
      for (i <- 0 until copies) {
        newArrangement.appendAll(arrangement)
        if (i < copies - 1)
          newArrangement.append(Condition.Unknown)
      }

      Record(newArrangement.toVector, newGroups)
    }
  }

  def solvePart1(input: List[Record]): Long = solve(input)

  def solvePart2(input: List[Record]): Long = solve(input.map(_.unfold(5)))

  private def solve(records: List[Record]): Long =
    records.map(numberOfArrangements).sum

  private def numberOfArrangements(record: Record): Long = {
    val arrangement = record.arrangement
    val groups = record.damagedGroups

    val dp = Array.ofDim[Long](arrangement.length + 1, groups.length + 1, groups.max + 1)

    dp(arrangement.length)(groups.length)(0) = 1
    dp(arrangement.length)(groups.length - 1)(groups.last) = 1

    for (pos <- arrangement.indices.reverse) {
      val condition = arrangement(pos)

      for (groupPos <- 0 to groups.length) {
        val maxCount = if (groupPos < groups.length) groups(groupPos) else 0

        for (count <- 0 to maxCount) {

          condition match {
            case Condition.Damaged =>

              if (groupPos < groups.length && count < groups(groupPos))
                dp(pos)(groupPos)(count) = dp(pos + 1)(groupPos)(count + 1)

            case Condition.Operational =>

              if (groupPos < groups.length && count == groups(groupPos))
                dp(pos)(groupPos)(count) = dp(pos + 1)(groupPos + 1)(0)
              else if (count == 0)
                dp(pos)(groupPos)(count) = dp(pos + 1)(groupPos)(0)

            case Condition.Unknown =>

              // if damaged
              if (groupPos < groups.length && count < groups(groupPos))
                dp(pos)(groupPos)(count) = dp(pos + 1)(groupPos)(count + 1)

              // if operational
              if (groupPos < groups.length && count == groups(groupPos))
                dp(pos)(groupPos)(count) += dp(pos + 1)(groupPos + 1)(0)
              else if (count == 0)
                dp(pos)(groupPos)(count) += dp(pos + 1)(groupPos)(0)
          }

        }
      }
    }

    dp(0)(0)(0)
  }

  private def parseRecord(line: String): Record = line match {
    case s"$arrangementRaw $groupsRaw" =>
      val arrangement = arrangementRaw.map {
        case '.' => Condition.Operational
        case '#' => Condition.Damaged
        case '?' => Condition.Unknown
      }.toVector

      val groups = groupsRaw.split(",").map(_.toInt).toVector

      Record(arrangement, groups)
  }

  private val sample =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1""".stripMargin
      .split("\n")
      .toList
      .map(parseRecord)

  private val input = Input.asList("day12.txt").map(parseRecord)

  println(solvePart1(sample)) // 21
  println(solvePart1(input)) // 7460

  println(solvePart2(sample)) // 525152
  println(solvePart2(input)) // 6720660274964


}
