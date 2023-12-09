package aoc

import scala.annotation.tailrec

object Day9 extends App {

  def solvePart1(input: List[List[Int]]): Int = solve(input)

  def solvePart2(input: List[List[Int]]): Int = solve(input.map(_.reverse))

  private def solve(input: List[List[Int]]): Int =
    input.map(interpolateNext).sum

  private def interpolateNext(input: List[Int]) = {
    @tailrec
    def loop(input: List[Int], answer: Int): Int =
      if (input.forall(_ == 0)) answer
      else {
        val diffs = input
          .sliding(2)
          .map { case List(first, second) => second - first }
          .toList

        loop(diffs, answer + diffs.last)
      }

    loop(input, input.last)
  }

  private val sample =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45""".stripMargin
      .split("\n")
      .toList
      .map(parseInput)

  private def parseInput(line: String) =
    line.split(" ").map(_.toInt).toList

  private val input = Input.asList("day9.txt").map(parseInput)

  println(solvePart1(sample)) // 114
  println(solvePart1(input)) // 1974232246

  println(solvePart2(sample)) // 2
  println(solvePart2(input)) // 928

}
