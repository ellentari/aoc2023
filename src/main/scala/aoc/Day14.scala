package aoc

import aoc.util.Grid
import aoc.util.Grid.Index

import scala.annotation.tailrec

object Day14 extends App {

  sealed trait Cell
  object Cell {
    case object RoundedRock extends Cell
    case object CubeRock extends Cell
    case object Empty extends Cell
  }

  case class Platform(grid: Grid[Cell]) {

    def totalLoad: Int = {
      def load(index: Index): Int = grid.height - index.row

      grid.indices
        .filter(grid(_) == Cell.RoundedRock)
        .map(load)
        .sum
    }

    def spinCycle: Platform = tiltNorth.tiltWest.tiltSouth.tiltEast

    def tiltNorth: Platform = Platform(tiltNorth(grid))

    def tiltSouth: Platform = Platform(tiltNorth(grid.flipHorizontally).flipHorizontally)

    def tiltWest: Platform = Platform(tiltWest(grid))

    def tiltEast: Platform = Platform(tiltWest(grid.flipVertically).flipVertically)

    private def tiltNorth(grid: Grid[Cell]): Grid[Cell] = {
      val updGrid = grid.rows.map(_.toArray).toArray

      for (idx <- grid.indices if grid(idx) == Cell.RoundedRock) {
        val firstObstacle = grid.shootUp(idx.row).find(updGrid(_)(idx.column) != Cell.Empty)
        val start = firstObstacle.getOrElse(0)

        val newRow = (start until idx.row).find(updGrid(_)(idx.column) == Cell.Empty)

        newRow.foreach { r =>
          updGrid(idx.row)(idx.column) = Cell.Empty
          updGrid(r)(idx.column) = Cell.RoundedRock
        }
      }

      Grid(updGrid.map(_.toIndexedSeq).toIndexedSeq)
    }

    private def tiltWest(grid: Grid[Cell]): Grid[Cell] = {
      val updGrid = grid.rows.map(_.toArray).toArray

      for (idx <- grid.indices if grid(idx) == Cell.RoundedRock) {
        val firstObstacle = grid.shootLeft(idx.column).find(updGrid(idx.row)(_) != Cell.Empty)
        val start = firstObstacle.getOrElse(0)
        val newCol = (start until idx.column).find(updGrid(idx.row)(_) == Cell.Empty)

        newCol.foreach { c =>
          updGrid(idx.row)(idx.column) = Cell.Empty
          updGrid(idx.row)(c) = Cell.RoundedRock
        }
      }

      Grid(updGrid.map(_.toIndexedSeq).toIndexedSeq)
    }

    override def toString: String =
      grid.format(_.map {
        case Cell.RoundedRock => 'O'
        case Cell.CubeRock => '#'
        case Cell.Empty => '.'
      }.mkString)
  }

  def solvePart1(platform: Platform): Int =
    platform
      .tiltNorth
      .totalLoad

  def solvePart2(platform: Platform, n: Int): Int = {
    @tailrec
    def loop(i: Int, current: Platform, processed: Map[Platform, Int], loads: Map[Int, Int]): Int =
      processed.get(current) match {
        case Some(cycleStart) =>
          val cycleLength = i - cycleStart
          val nAfterCycleStart = n - cycleStart - 1
          val loadI = cycleStart + (nAfterCycleStart % cycleLength)

          loads(loadI)
        case None =>
          val next = current.spinCycle

          loop(i + 1, next, processed.updated(current, i), loads.updated(i, next.totalLoad))
      }

    loop(0, platform, Map.empty, Map.empty)
  }

  private def parsePlatform(input: String): Platform = {

    def parseCell(cell: Char): Cell = cell match {
      case '.' => Cell.Empty
      case '#' => Cell.CubeRock
      case 'O' => Cell.RoundedRock
    }

    val grid = Grid.parseCharacterGrid(input)

    Platform(grid.map(parseCell))
  }

  private val sample = parsePlatform(
    """O....#....
      |O.OO#....#
      |.....##...
      |OO.#O....O
      |.O.....O#.
      |O.#..O.#.#
      |..O..#O..O
      |.......O..
      |#....###..
      |#OO..#....""".stripMargin
  )

  private val input = parsePlatform(Input.asString("day14.txt"))

  println(solvePart1(sample)) // 136
  println(solvePart1(input)) // 108935

  println(solvePart2(sample, n = 1000000000)) // 64
  println(solvePart2(input, n = 1000000000)) // 100876

}
