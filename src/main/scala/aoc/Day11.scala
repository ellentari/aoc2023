package aoc

import aoc.util.Grid
import aoc.util.Grid.Index

object Day11 extends App {

  sealed trait Cell
  object Cell {
    case object Empty extends Cell
    case object Universe extends Cell
  }

  def solve(grid: Grid[Cell], expand: Int): Long = {
    val columnsToExpand = getIndicesWithoutUniverses(grid.columns)
    val rowsToExpand = getIndicesWithoutUniverses(grid.rows)

    val universes = grid.indices.filter(grid(_) == Cell.Universe).toVector

    (for {
      i <- universes.indices
      j <- i + 1 until universes.length
      dist = distance(universes(i), universes(j), expand, rowsToExpand, columnsToExpand)
    } yield dist).sum
  }

  private def getIndicesWithoutUniverses(rows: IndexedSeq[IndexedSeq[Cell]]): Set[Int] =
    rows
      .zipWithIndex
      .filter { case (row, _) => !row.contains(Cell.Universe) }
      .map(_._2)
      .toSet

  private def distance(i1: Index, i2: Index, expand: Int, expandedRows: Set[Int], expandedCols: Set[Int]): Long = {
    def expandedDistance(min: Int, max: Int, expandedIs: Set[Int]): Long = {
      val expandedCount = (min until max).count(expandedIs.contains)
      max - min + expandedCount.toLong * (expand - 1)
    }

    val rowDist = expandedDistance(i1.row min i2.row, i1.row max i2.row, expandedRows)
    val colDist = expandedDistance(i1.column min i2.column, i1.column max i2.column, expandedCols)

    rowDist + colDist
  }

  private def parseGrid(input: String): Grid[Cell] = {
    def parseCell(char: Char): Cell = char match {
      case '.' => Cell.Empty
      case '#' => Cell.Universe
    }

    Grid.parseCharacterGrid(input).map(parseCell)
  }

  private val sample = parseGrid(
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....""".stripMargin
  )

  private val input = parseGrid(Input.asString("day11.txt"))

  println(solve(sample, 2)) // 374
  println(solve(input, 2)) // 9509330

  println(solve(sample, 10)) // 1030
  println(solve(sample, 100)) // 8410
  println(solve(input, 1000000)) // 635832237682

}
