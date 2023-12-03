package aoc.util

import aoc.util.Grid.Index

case class Grid[A](rows: IndexedSeq[IndexedSeq[A]]) {
  
  def height: Int = rows.length
  def width: Int = rows.headOption.fold(0)(_.length)

  def apply(index: Index): A = apply(index.row, index.column)
  def apply(row: Int, col: Int): A = rows(row)(col)

  def indices: List[Index] = rows.indices.flatMap(row => rows(row).indices.map(Index(row, _))).toList

  def adjacent4(index: Index): List[Index] = adjacent4(index.row, index.column)
  def adjacent4(row: Int, col: Int): List[Index] =
    List(
      (row - 1, col),
      (row, col + 1),
      (row + 1, col),
      (row, col - 1)
    )
      .filter(isWithinGrid)
      .map((Index.apply _).tupled)

  def adjacent8(index: Index): List[Index] = adjacent8(index.row, index.column)
  def adjacent8(row: Int, col: Int): List[Index] =
    List(
      (row - 1, col),
      (row - 1, col + 1),
      (row, col + 1),
      (row + 1, col + 1),
      (row + 1, col),
      (row + 1, col - 1),
      (row, col - 1),
      (row - 1, col - 1)
    )
      .filter(isWithinGrid)
      .map((Index.apply _).tupled)

  private def isWithinGrid(cc: (Int, Int)) =
    cc._1 >= 0 && cc._1 < rows.length && cc._2 >= 0 && cc._2 < rows(cc._1).length

}

object Grid {

  case class Index(row: Int, column: Int)

  def parseCharacterGrid(raw: String): Grid[Char] =
    Grid(raw.split("\n").map(_.toVector).toVector)

}
