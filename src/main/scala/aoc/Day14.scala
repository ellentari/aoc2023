package aoc

import aoc.util.Grid
import aoc.util.Grid.Index

object Day14 extends App {

  sealed trait Cell
  object Cell {
    case object RoundedRock extends Cell
    case object CubeRock extends Cell
    case object Empty extends Cell
  }

  case class Platform(grid: Grid[Cell]) {

    def tiltNorth: Platform = ???

  }

  type RawInput = String

  def solvePart1(input: RawInput) = {
    val grid = Grid.parseCharacterGrid(input)

    val tilted = tiltNorth(grid)

    //    println(result.format(_.mkString))

    tilted.indices.filter(tilted(_) == 'O')
      .map(idx => tilted.height - idx.row)
      .sum
  }

  private def tiltNorth(grid: Grid[Char]): Grid[Char] = {
    var updGrid = grid

    for (idx <- grid.indices if grid(idx) == 'O') {
      val firstObstacle = (idx.row - 1 to 0 by - 1)
        .find(r => updGrid(r, idx.column) != '.')

      val newRow = (firstObstacle.getOrElse(0) until idx.row)
        .find(r => updGrid(r, idx.column) == '.')
        .getOrElse(idx.row)

      updGrid = updGrid.updated(idx, '.')
      updGrid = updGrid.updated(Index(newRow, idx.column), 'O')
    }

    updGrid
  }

  private def tileWest(grid: Grid[Char]): Grid[Char] = {
    var updGrid = grid

    for (idx <- grid.indices if grid(idx) == 'O') {
      val firstObstacle = (idx.column - 1 to 0 by -1)
        .find(c => updGrid(idx.row, c) != '.')

      val newCol = (firstObstacle.getOrElse(0) until idx.column)
        .find(c => updGrid(idx.row, c) == '.')
        .getOrElse(idx.column)

      updGrid = updGrid.updated(idx, '.')
      updGrid = updGrid.updated(Index(idx.row, newCol), 'O')
    }

    updGrid
  }

  private def tiltSouth(grid: Grid[Char]): Grid[Char] = {
    var updGrid = grid

    for (idx <- grid.indices.reverse if grid(idx) == 'O') {
      val firstObstacle = (idx.row + 1 until grid.height)
        .find(r => updGrid(r, idx.column) != '.')

      val newRow = (firstObstacle.getOrElse(grid.height - 1) until idx.row by -1)
        .find(r => updGrid(r, idx.column) == '.')
        .getOrElse(idx.row)

      updGrid = updGrid.updated(idx, '.')
      updGrid = updGrid.updated(Index(newRow, idx.column), 'O')
    }

    updGrid
  }

  private def tiltEast(grid: Grid[Char]): Grid[Char] = {
    var updGrid = grid

    for (idx <- grid.indices.reverse if grid(idx) == 'O') {
      val firstObstacle = (idx.column + 1 until grid.width)
        .find(c => updGrid(idx.row, c) != '.')

      val newCol = (firstObstacle.getOrElse(grid.width - 1) until idx.column by -1)
        .find(c => updGrid(idx.row, c) == '.')
        .getOrElse(idx.column)

      updGrid = updGrid.updated(idx, '.')
      updGrid = updGrid.updated(Index(idx.row, newCol), 'O')
    }

    updGrid
  }

  def solvePart2(input: RawInput) = {
    var grid = Grid.parseCharacterGrid(input)

//    val cycles = 1
    val cycles = 1000000000

    var totalLoad = 0

    val cache =
      scala.collection.mutable.HashMap.empty[Grid[Char], (Int, Int, Grid[Char])]

    val loads =
      scala.collection.mutable.HashMap.empty[Int, Int]

    var i = 0
    var continue = true

    var cycleStart = -1
    var cycleLength = 0

    while (i < cycles && continue) {
//      if (cache.contains(grid)) {
//        val (j, load, nextGrid) = cache(grid)
//
//        println(i + " " + load + s" (from cache ${j})")
//
//        totalLoad += load
//        grid = nextGrid
//      } else {

        val tilted = tiltNorth(grid)
        val tilted2 = tileWest(tilted)
        val tilted3 = tiltSouth(tilted2)
        val tilted4 = tiltEast(tilted3)

        if (cache.contains(tilted4)) {
          continue = false
          cycleStart = cache(tilted4)._1
          cycleLength = i - cycleStart + 1
        } else {
          val load = countLoad(tilted4)

          totalLoad += load
          cache.update(grid, (i, load, tilted4))
          loads.update(i, load)

          println(i + " " + load)

          grid = tilted4
        }

//      }

      i += 1
    }

    val cyclesCount = cycles - cycleStart - 1

    println("cycleStart " + cycleStart)
    println("cyclesCount " + cyclesCount)
    println("cycleLength " + cycleLength)
    println("mod " + (cyclesCount % cycleLength))


    loads((cycleStart + (cyclesCount % cycleLength)))
  }

  private def countLoad(tilted: Grid[Char]) = {
    tilted.indices.filter(tilted(_) == 'O')
      .map(idx => tilted.height - idx.row)
      .sum
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

  private val sample =
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

    private val input = Input.asString("day14.txt")
//  private val input = sample

//  println("==================== Part 1 ====================")
//  println("Example: " + solvePart1(sample))
//  AnswerSubmitter.promptAndSubmitPart1(Day, solvePart1(input))

//  println("==================== Part 2 ====================")
//  println("Example: " + solvePart2(sample))
//  AnswerSubmitter.promptAndSubmitPart2(Day, solvePart2(input))
//  println("================================================")

}
