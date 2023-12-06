package aoc

import scala.annotation.tailrec

object Day6 extends App {

  case class Input(times: List[Int], distances: List[Int]) {
    def recordsPart1: List[Race] = times.zip(distances).map { case (t, d) => Race(t, d) }
    def recordsPart2: List[Race] = {
      val time = times.mkString.toInt
      val distance = distances.mkString.toLong

      List(Race(time, distance))
    }
  }

  case class Race(time: Int, distance: Long)

  def solve(records: List[Race]): Int = {

    def countWaysToBeatRecord(record: Race): Int = {
      val minTime = findMinButtonHoldTimeToBeatTheRecord(record)
      val maxTime = findMaxButtonHoldTimeToBeatTheRecord(record)

      maxTime - minTime + 1
    }

    records
      .map(countWaysToBeatRecord)
      .product
  }

  private def findMinButtonHoldTimeToBeatTheRecord(record: Race): Int = {
    @tailrec
    def loop(low: Int, high: Int): Int =
      if (low >= high) high
      else {
        val mid = low + (high - low) / 2
        val distance = calculateDistance(mid, record.time)
        if (distance <= record.distance) loop(mid + 1, high)
        else loop(low, mid)
      }

    loop(0, record.time)
  }

  private def findMaxButtonHoldTimeToBeatTheRecord(record: Race): Int = {
    @tailrec
    def loop(low: Int, high: Int): Int = {
      if (low >= high - 1) low
      else {
        val mid = low + (high - low) / 2
        val distance = calculateDistance(mid, record.time)
        if (distance > record.distance) loop(mid, high)
        else loop(low, mid - 1)
      }
    }

    loop(0, record.time)
  }

  private def calculateDistance(buttonHoldTime: Int, totalTime: Int): Long = {
    val speed = buttonHoldTime
    val remainingTime = totalTime - buttonHoldTime

    remainingTime.toLong * speed
  }

  private def parseInput(input: List[String]): Input = {

    def parseRow(row: String): List[Int] =
      row.split(":")(1).trim.split("\\s+").map(_.toInt).toList

    val time = parseRow(input.head)
    val distance = parseRow(input.tail.head)

    Input(time, distance)
  }

  private val sample = parseInput(
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin
      .split("\n")
      .toList
  )

  private val input = parseInput(aoc.Input.asList("day6.txt"))

  println(solve(sample.recordsPart1)) // 288
  println(solve(input.recordsPart1)) // 2344708

  println(solve(sample.recordsPart2)) // 71503
  println(solve(input.recordsPart2)) // 30125202

}
