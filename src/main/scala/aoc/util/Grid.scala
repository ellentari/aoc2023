package aoc.util

import aoc.util.Grid.Index

case class Grid[A](rows: IndexedSeq[IndexedSeq[A]]) {

  def height: Int = rows.length
  def width: Int = rows.headOption.fold(0)(_.length)

  def apply(index: Index): A = apply(index.row, index.column)
  def apply(row: Int, col: Int): A = rows(row)(col)

  def map[B](f: A => B): Grid[B] = Grid(rows.map(_.map(f)))

  def updated(index: Index, a: A): Grid[A] = updated(index.row, index.column, a)
  def updated(row: Int, col: Int, a: A): Grid[A] = updated(row, col, _ => a)
  def updated(index: Index, f: A => A): Grid[A] = updated(index.row, index.column, f)
  def updated(row: Int, col: Int, f: A => A): Grid[A] =
    Grid(rows.updated(row, rows(row).updated(col, f(apply(row, col)))))

  def indices: List[Index] = rows.indices.flatMap(row => rows(row).indices.map(Index(row, _))).toList

  def bordersIndices: IndexedSeq[Index] =
    (topSideIndices ++ bottomSideIndices ++ rightSideIndices ++ leftSideIndices).distinct
  def topSideIndices: IndexedSeq[Index] = columnIndices.map(Index(0, _))
  def bottomSideIndices: IndexedSeq[Index] = columnIndices.map(Index(height - 1, _))
  def rightSideIndices: IndexedSeq[Index] = rowIndices.map(Index(_, 0))
  def leftSideIndices: IndexedSeq[Index] = rowIndices.map(Index(_, width - 1))

  private def rowIndices: Range = rows.indices
  private def columnIndices: Range = 0 until width

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

  def adjacent(index: Index, direction: Direction): Option[Index] =
    direction match {
      case Direction.South => bottom(index)
      case Direction.North => top(index)
      case Direction.East => right(index)
      case Direction.West => left(index)
    }

  def top(index: Index): Option[Index] = {
    val next = Index(index.row - 1, index.column)
    Option.when(isWithinGrid(next))(next)
  }

  def bottom(index: Index): Option[Index] = {
    val next = Index(index.row + 1, index.column)
    Option.when(isWithinGrid(next))(next)
  }

  def left(index: Index): Option[Index] = {
    val next = Index(index.row, index.column - 1)
    Option.when(isWithinGrid(next))(next)
  }

  def right(index: Index): Option[Index] = {
    val next = Index(index.row, index.column + 1)
    Option.when(isWithinGrid(next))(next)
  }

  private def isWithinGrid(cc: (Int, Int)) =
    cc._1 >= 0 && cc._1 < rows.length && cc._2 >= 0 && cc._2 < rows(cc._1).length

  private def isWithinGrid(cc: Index): Boolean = isWithinGrid((cc.row, cc.column))

}

object Grid {

  case class Index(row: Int, column: Int) {
    def map(f: Int => Int): Index =
      Index(f(row), f(column))
  }

  def parseCharacterGrid(raw: String): Grid[Char] =
    Grid(raw.split("\n").map(_.toVector).toVector)

}
