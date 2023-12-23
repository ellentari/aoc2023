package aoc

import aoc.algo.BFS
import aoc.util.Grid
import aoc.util.Grid.Index

import scala.annotation.tailrec
import scala.collection.mutable

object Day23 extends App {

  sealed trait Cell

  object Cell {

    case object Path extends Cell
    case object Forest extends Cell

    sealed trait Slope extends Cell

    object Left extends Slope
    object Right extends Slope
    object Up extends Slope
    object Down extends Slope
  }

  import Cell._

  def solvePart1(grid: Grid[Cell]): Int = {
    def getAdjacent(c: Index): List[Index] = {
      grid(c) match {
        case Path => grid.adjacent4(c)
        case Right => grid.right(c).toList
        case Left => grid.left(c).toList
        case Up => grid.top(c).toList
        case Down => grid.bottom(c).toList
      }
    }

    solve(grid, getAdjacent)
  }

  def solvePart2(grid: Grid[Cell]): Int = solve(grid, grid.adjacent4)

  private def solve(grid: Grid[Cell], getAdjacent: Index => List[Index]): Int = {
    val start = grid.topSideIndices.find(grid(_) == Path).get
    val end = grid.bottomSideIndices.find(grid(_) == Path).get

    val adjacencyList = buildGraph(grid, getAdjacent)

    findLongestPath(start, end)(adjacencyList).get
  }

  private def buildGraph(grid: Grid[Cell], getAdjacent: Index => List[Index]): Map[Index, List[(Index, Int)]] = {
    def isCrossroad(idx: Index): Boolean = getAdjacent(idx).count(canStep) > 2

    def canStep(idx: Index): Boolean = grid(idx) != Forest

    val crossroads = grid.indices.filter(canStep).filter(isCrossroad)
    val start = grid.topSideIndices.find(canStep).get
    val end = grid.bottomSideIndices.find(canStep).get

    val vertices = start +: crossroads :+ end
    val verticesSet = vertices.toSet

    val graph = vertices
      .map(v => v -> BFS.shortestPathLengthsToAll(v)(getAdjacent(_).filter(canStep), verticesSet.contains))
      .toMap

    graph
  }

  private def findLongestPath(start: Index, end: Index)(adjacencyList: Map[Index, List[(Index, Int)]]): Option[Int] = {
    val onPath = mutable.HashSet.empty[Index]

    def loop(current: Index, pathLength: Int): Option[Int] = {
      if (current == end) Some(pathLength)
      else {
        onPath.add(current)

        val result = adjacencyList.getOrElse(current, Nil)
          .filterNot { case (n, _) => onPath.contains(n) }
          .flatMap { case (n, c) => loop(n, pathLength + c) }
          .maxOption

        onPath.remove(current)

        result
      }
    }

    loop(start, 0)
  }

  private def parseGrid(s: String): Grid[Cell] =
    Grid.parseCharacterGrid(s)
      .map {
        case '.' => Path
        case '#' => Forest
        case '^' => Up
        case '>' => Right
        case 'v' => Down
        case '<' => Left
      }

  private val sample = parseGrid(
    """#.#####################
      |#.......#########...###
      |#######.#########.#.###
      |###.....#.>.>.###.#.###
      |###v#####.#v#.###.#.###
      |###.>...#.#.#.....#...#
      |###v###.#.#.#########.#
      |###...#.#.#.......#...#
      |#####.#.#.#######.#.###
      |#.....#.#.#.......#...#
      |#.#####.#.#.#########v#
      |#.#...#...#...###...>.#
      |#.#.#v#######v###.###v#
      |#...#.>.#...>.>.#.###.#
      |#####v#.#.###v#.#.###.#
      |#.....#...#...#.#.#...#
      |#.#########.###.#.#.###
      |#...###...#...#...#.###
      |###.###.#.###v#####v###
      |#...#...#.#.>.>.#.>.###
      |#.###.###.#.###.#.#v###
      |#.....###...###...#...#
      |#####################.#""".stripMargin
  )

  private val input = parseGrid(Input.asString("day23.txt"))

  println(solvePart1(sample)) // 94
  println(solvePart1(input)) // 2110

  println(solvePart2(sample)) // 154
  println(solvePart2(input)) // 6514

}
