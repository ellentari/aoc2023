package aoc

import aoc.Day10.Cell.Pipe
import aoc.algo.BFS
import aoc.util.Direction._
import aoc.util.Grid.Index
import aoc.util.{Direction, Grid}

object Day10 extends App {

  sealed trait Cell

  object Cell {
    case object Ground extends Cell
    case object Empty extends Cell
    case object Start extends Cell
    case object Outside extends Cell

    case class Pipe(output: List[Direction], symbol: Char) extends Cell {
      def canConnectTo(otherPipe: Pipe): Boolean = {
        val oppositePipeDirections = otherPipe.output.map(_.opposite)
        output.exists(oppositePipeDirections.contains)
      }
    }
  }

  def solvePart1(grid: Grid[Cell]): Int = {
    val (start, updatedGrid) = figureOutStartPipe(grid)

    BFS.maxDistance(start)(getAdjacentPipes(updatedGrid, _))
  }

  def solvePart2(grid: Grid[Cell]): Int = {
    val (start, updatedGrid) = figureOutStartPipe(grid)

    val pipes = BFS.discoverRegion(start)(getAdjacentPipes(updatedGrid, _)).toSet

    val (scaledGrid, scaledPipes) = scaleGrid(updatedGrid, pipes)

    val marked = markOutside(scaledGrid, scaledPipes)

    marked.indices
      .count(idx => marked(idx) != Cell.Outside && marked(idx) != Cell.Empty && !scaledPipes.contains(idx))
  }

  private def figureOutStartPipe(grid: Grid[Cell]): (Index, Grid[Cell]) = {
    val start = grid.indices.find(grid(_) == Cell.Start).get

    val possibleDirections = List(North, South, West, East)
      .filter { dir =>
        grid.adjacent(start, dir)
          .map(grid(_))
          .exists {
            case p1: Pipe if p1.canConnectTo(Pipe(List(dir), '?')) => true
            case _ => false
          }
      }

    start -> grid.updated(start, Pipe(possibleDirections, 'S'))
  }

  private def getAdjacentPipes(grid: Grid[Cell], current: Index): List[Index] =
    grid(current) match {
      case p@Cell.Pipe(output, _) =>
        output
          .flatMap(grid.adjacent(current, _))
          .map(i => i -> grid(i))
          .collect {
            case (i, p1: Pipe) if p.canConnectTo(p1) => i
          }
    }

  private def scaleGrid(grid: Grid[Cell], pipes: Set[Index]): (Grid[Cell], Set[Index]) = {
    val fillEmpty = Cell.Empty
    val scaleFactor = 2

    val scaled = Array.fill[Cell](grid.height * scaleFactor, grid.width * scaleFactor)(fillEmpty)

    grid.indices
      .foreach { index =>
        val scaledIndex = index.map(_ * scaleFactor)

        scaled(scaledIndex.row)(scaledIndex.column) = grid(index)
      }

    val scaledPipes = scala.collection.mutable.HashSet.from(pipes.map(_.map(_ * scaleFactor)))

    var scaledGrid = Grid(scaled.map(_.toVector).toVector)

    def pipeAt(idx: Index): Option[Pipe] =
      List(scaledGrid(idx)).collectFirst { case p: Pipe => p }

    val verticalPipe = Pipe(List(North, South), '|')
    val horizontalPipe = Pipe(List(West, East), '-')

    for (index <- scaledGrid.indices if scaledGrid(index) == fillEmpty) {
      val left = scaledGrid.left(index).flatMap(pipeAt)
      val right = scaledGrid.right(index).flatMap(pipeAt)

      if (left.exists(_.output.contains(East)) && right.exists(_.output.contains(West))) {
        scaledGrid = scaledGrid.updated(index, horizontalPipe)
        scaledPipes.add(index)
      }

      val top = scaledGrid.top(index).flatMap(pipeAt)
      val bottom = scaledGrid.bottom(index).flatMap(pipeAt)

      if (top.exists(_.output.contains(South)) && bottom.exists(_.output.contains(North))) {
        scaledGrid = scaledGrid.updated(index, verticalPipe)
        scaledPipes.add(index)
      }
    }

    scaledGrid -> scaledPipes.toSet
  }

  private def markOutside(grid: Grid[Cell], pipes: Set[Index]): Grid[Cell] = {
    val borders = grid.bordersIndices.filterNot(pipes.contains)

    var markedGrid = grid

    BFS.visitAll(borders)(
      grid.adjacent8(_).filterNot(pipes.contains),
      (idx, _) => markedGrid = markedGrid.updated(idx, Cell.Outside)
    )

    markedGrid
  }

  private def parseCell(cell: Char): Cell = cell match {
    case '.' => Cell.Ground
    case '|' => Cell.Pipe(List(North, South), cell)
    case '-' => Cell.Pipe(List(West, East), cell)
    case 'L' => Cell.Pipe(List(North, East), cell)
    case 'J' => Cell.Pipe(List(North, West), cell)
    case '7' => Cell.Pipe(List(South, West), cell)
    case 'F' => Cell.Pipe(List(South, East), cell)
    case 'S' => Cell.Start
  }

  private val part1Sample1 = Grid.parseCharacterGrid(
    """.....
      |.S-7.
      |.|.|.
      |.L-J.
      |.....""".stripMargin
  ).map(parseCell)

  private val part1Sample2 = Grid.parseCharacterGrid(
    """..F7.
      |.FJ|.
      |SJ.L7
      ||F--J
      |LJ...""".stripMargin
  ).map(parseCell)

  private val part2Sample1 = Grid.parseCharacterGrid(
    """...........
      |.S-------7.
      |.|F-----7|.
      |.||.....||.
      |.||.....||.
      |.|L-7.F-J|.
      |.|..|.|..|.
      |.L--J.L--J.
      |...........""".stripMargin
  ).map(parseCell)

  private val part2Sample2 = Grid.parseCharacterGrid(
    """..........
      |.S------7.
      |.|F----7|.
      |.||....||.
      |.||....||.
      |.|L-7F-J|.
      |.|..||..|.
      |.L--JL--J.
      |..........""".stripMargin
  ).map(parseCell)

  private val part2Sample3 = Grid.parseCharacterGrid(
    """.F----7F7F7F7F-7....
      |.|F--7||||||||FJ....
      |.||.FJ||||||||L7....
      |FJL7L7LJLJ||LJ.L-7..
      |L--J.L7...LJS7F-7L7.
      |....F-J..F7FJ|L7L7L7
      |....L7.F7||L7|.L7L7|
      |.....|FJLJ|FJ|F7|.LJ
      |....FJL-7.||.||||...
      |....L---J.LJ.LJLJ...""".stripMargin
  ).map(parseCell)

  private val part2Sample4 = Grid.parseCharacterGrid(
    """FF7FSF7F7F7F7F7F---7
      |L|LJ||||||||||||F--J
      |FL-7LJLJ||||||LJL-77
      |F--JF--7||LJLJ7F7FJ-
      |L---JF-JLJ.||-FJLJJ7
      ||F|F-JF---7F7-L7L|7|
      ||FFJF7L7F-JF7|JL---7
      |7-L-JL7||F7|L7F-7F7|
      |L.L7LFJ|||||FJL7||LJ
      |L7JLJL-JLJLJL--JLJ.L""".stripMargin
  ).map(parseCell)

  private val input = Grid.parseCharacterGrid(Input.asString("day10.txt")).map(parseCell)

  println(solvePart1(part1Sample1)) // 4
  println(solvePart1(part1Sample2)) // 8
  println(solvePart1(input)) // 6800

  println(solvePart2(part2Sample1)) // 4
  println(solvePart2(part2Sample2)) // 4
  println(solvePart2(part2Sample3)) // 8
  println(solvePart2(part2Sample4)) // 10
  println(solvePart2(input)) // 483

}
