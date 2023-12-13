package aoc

import aoc.util.Grid

object Day13 extends App {

  def solvePart1(input: List[Grid[Char]]): Int = solve(input, diff = 0)

  def solvePart2(input: List[Grid[Char]]): Int = solve(input, diff = 1)

  private def solve(input: List[Grid[Char]], diff: Int): Int =
    input
      .map { grid =>
        val row = findHorizontalReflection(grid, diff).getOrElse(0)
        val col = findVerticalReflection(grid, diff).getOrElse(0)

        100 * row + col
      }
      .sum

  private def findHorizontalReflection(grid: Grid[Char], diff: Int): Option[Int] =
    findVerticalReflection(grid.transpose, diff)

  private def findVerticalReflection(grid: Grid[Char], diff: Int): Option[Int] =
    (1 until grid.width)
      .find { column =>
        val sideSize = column min (grid.width - column)

        val left = grid.sliceVertically(column - sideSize, column)
        val right = grid.sliceVertically(column, column + sideSize)

        areDifferentWithDiff(left, right.flipVertically, diff)
      }

  private def areDifferentWithDiff(grid1: Grid[Char], grid2: Grid[Char], diff: Int): Boolean = {
    var diffCount = 0

    for (idx <- grid1.indices) {
      if (grid1(idx) != grid2(idx))
        diffCount += 1

      if (diffCount > diff)
        return false
    }

    diffCount == diff
  }

  private def parseGrids(input: String): List[Grid[Char]] =
    input.split("\n\n").map(Grid.parseCharacterGrid).toList

  private val sample = parseGrids(
    """#.##..##.
      |..#.##.#.
      |##......#
      |##......#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.
      |
      |#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#""".stripMargin)

  private val input = parseGrids(Input.asString("day13.txt"))

  println(solvePart1(sample)) // 405
  println(solvePart1(input)) // 30535

  println(solvePart2(sample)) // 400
  println(solvePart2(input)) // 30844

}
