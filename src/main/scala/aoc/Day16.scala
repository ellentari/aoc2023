package aoc

import aoc.algo.BFS
import aoc.util.Direction.{East, North, South, West}
import aoc.util.Grid.Index
import aoc.util.{Direction, Grid}

object Day16 extends App {

  sealed trait Cell
  object Cell {
    case object Empty extends Cell

    sealed trait Mirror extends Cell
    object `/` extends Mirror
    object `\\` extends Mirror

    sealed trait Splitter extends Cell
    object `|` extends Splitter
    object `-` extends Splitter
  }

  import Cell._

  def solvePart1(grid: Grid[Cell]): Int = getEnergyLevel(grid, Index(0, 0), East)

  def solvePart2(grid: Grid[Cell]): Int =
    List(
      grid.leftSideIndices.map(_ -> East),
      grid.rightSideIndices.map(_ -> West),
      grid.topSideIndices.map(_ -> South),
      grid.bottomSideIndices.map(_ -> North)
    ).flatten
      .map { case (idx, dir) => getEnergyLevel(grid, idx, dir) }
      .max

  private def getEnergyLevel(grid: Grid[Cell], start: Index, startDir: Direction): Int = {
    val set = scala.collection.mutable.HashSet.empty[Index]
    BFS.visitAll((start, startDir))((getNext(grid) _).tupled, t => set.add(t._1))
    set.size
  }

  private def getNext(grid: Grid[Cell])(index: Index, direction: Direction): List[(Index, Direction)] = {
    def mirror(mirror: Mirror): Direction =
      (mirror, direction) match {
        case (`/`, North | South) => direction.turnRight
        case (`/`, East | West) => direction.turnLeft
        case (`\\`, North | South) => direction.turnLeft
        case (`\\`, East | West) => direction.turnRight
      }

    def split(split: Splitter): List[Direction] =
      (split, direction) match {
        case (`|`, East | West) => List(North, South)
        case (`|`, North | South) => List(direction)
        case (`-`, North | South) => List(West, East)
        case (`-`, East | West) => List(direction)
      }

    val nextDirections = grid(index) match {
      case Empty => List(direction)
      case m: Mirror => List(mirror(m))
      case s: Splitter => split(s)
    }

    nextDirections.flatMap(dir => grid.adjacent(index, dir).map(_ -> dir))
  }

  private def parseGrid(s: String): Grid[Cell] =
    Grid.parseCharacterGrid(s).map {
      case '.' => Empty
      case '|' => `|`
      case '-' => `-`
      case '\\' => `\\`
      case '/' => `/`
    }

  private val sample = parseGrid(
    """.|...\....
      ||.-.\.....
      |.....|-...
      |........|.
      |..........
      |.........\
      |..../.\\..
      |.-.-/..|..
      |.|....-|.\
      |..//.|....""".stripMargin
  )

  private val input = parseGrid(Input.asString("day16.txt"))

  println(solvePart1(sample)) // 46
  println(solvePart1(input)) // 6855

  println(solvePart2(sample)) // 51
  println(solvePart2(input)) // 7513

}
