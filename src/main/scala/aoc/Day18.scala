package aoc

import aoc.algo.Polygon
import aoc.util._

object Day18 extends App {

  case class Instruction(direction: Direction, steps: Int)

  case class PlanEntry(direction: Direction, steps: Int, color: String) {

    def part1Instruction: Instruction = Instruction(direction, steps)

    def part2Instruction: Instruction = {
      val steps = Integer.parseInt(color.take(5), 16)
      val direction = color.last match {
        case '0' => Direction.East
        case '1' => Direction.South
        case '2' => Direction.West
        case '3' => Direction.North
      }

      Instruction(direction, steps)
    }
  }

  def solvePart1(input: List[PlanEntry]): Long = solve(input.map(_.part1Instruction))

  def solvePart2(input: List[PlanEntry]): Long = solve(input.map(_.part2Instruction))

  private def solve(entries: List[Instruction]): Long = {
    val start = Coordinate2D(1, 1)
    val coordinates = entries.scanLeft(start)((current, entry) => current.add(entry.steps, entry.direction))
      .init // drop last because it's same as first
      .toVector

    Polygon.areaIncludingBoundaries(coordinates)
  }

  private def parseEntry(line: String): PlanEntry = {
    line match {
      case s"$d $s (#$color)" =>
        val direction = d match {
          case "U" => Direction.North
          case "D" => Direction.South
          case "R" => Direction.East
          case "L" => Direction.West
        }

        PlanEntry(direction, s.toInt, color)
    }
  }

  private val sample =
    """R 6 (#70c710)
      |D 5 (#0dc571)
      |L 2 (#5713f0)
      |D 2 (#d2c081)
      |R 2 (#59c680)
      |D 2 (#411b91)
      |L 5 (#8ceee2)
      |U 2 (#caa173)
      |L 1 (#1b58a2)
      |U 2 (#caa171)
      |R 2 (#7807d2)
      |U 3 (#a77fa3)
      |L 2 (#015232)
      |U 2 (#7a21e3)""".stripMargin
      .split("\n")
      .toList
      .map(parseEntry)

  private val input = Input.asList("day18.txt").map(parseEntry)

  println(solvePart1(sample)) // 62
  println(solvePart1(input)) // 53300

  println(solvePart2(sample)) // 952408144115
  println(solvePart2(input)) // 64294334780659

}
