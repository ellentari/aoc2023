package aoc

import aoc.algo.Dijkstra
import aoc.util.Direction.{East, South}
import aoc.util.Grid.Index
import aoc.util.{Direction, Grid}

object Day17 extends App {

  private case class State(
    idx: Index,
    direction: Direction,
    steps: Int) {

    def nextStates(minStepsForward: Int, maxStepsForward: Int, grid: Grid[?]): List[State] =
      List(
        goForward(maxStepsForward, grid),
        turnLeft(minStepsForward, grid),
        turnRight(minStepsForward, grid),
      ).flatten

    def isTerminal(minStepsForward: Int, grid: Grid[?]): Boolean =
      canStop(minStepsForward) && idx == grid.bottomRightCorner

    private def canGoForward(maxStepsForward: Int): Boolean = steps < maxStepsForward
    private def canTurn(minStepsForward: Int): Boolean = steps >= minStepsForward
    private def canStop(minStepsForward: Int): Boolean = canTurn(minStepsForward)

    private def goForward(maxStepsForward: Int, grid: Grid[?]): Option[State] =
      Option.when(canGoForward(maxStepsForward))(grid.adjacent(idx, direction)).flatten
        .map(State(_, direction, steps + 1))

    private def turnLeft(minStepsForward: Int, grid: Grid[?]): Option[State] =
      turn(direction.turnLeft, minStepsForward, grid)

    private def turnRight(minStepsForward: Int, grid: Grid[?]): Option[State] =
      turn(direction.turnRight, minStepsForward, grid)

    private def turn(nextDirection: Direction, minStepsForward: Int, grid: Grid[?]): Option[State] =
      Option.when(canTurn(minStepsForward))(grid.adjacent(idx, nextDirection)).flatten
        .map(State(_, nextDirection, 1))

  }

  def solvePart1(grid: Grid[Int]): Int = solve(grid, minStepsForward = 0, maxStepsForward = 3)

  def solvePart2(grid: Grid[Int]): Int = solve(grid, minStepsForward = 4, maxStepsForward = 10)

  private def solve(grid: Grid[Int], minStepsForward: Int, maxStepsForward: Int): Int = {
    val init = List(East, South).map(State(grid.topLeftCorner, _, 0))

    Dijkstra.findMinPathCost(init :_*)(
      _.nextStates(minStepsForward, maxStepsForward, grid).map(s => s -> grid(s.idx)),
      _.isTerminal(minStepsForward, grid)
    ).get
  }

  private def parseGrid(s: String): Grid[Int] =
    Grid.parseCharacterGrid(s).map(_ - '0')

  private val sample = parseGrid(
    """2413432311323
      |3215453535623
      |3255245654254
      |3446585845452
      |4546657867536
      |1438598798454
      |4457876987766
      |3637877979653
      |4654967986887
      |4564679986453
      |1224686865563
      |2546548887735
      |4322674655533""".stripMargin
  )

  val sample2 = parseGrid(
    """111111111111
      |999999999991
      |999999999991
      |999999999991
      |999999999991
      |""".stripMargin
  )

  private val input = parseGrid(Input.asString("day17.txt"))

  println(solvePart1(sample)) // 102
  println(solvePart1(input)) // 970

  println(solvePart2(sample)) // 94
  println(solvePart2(sample2)) // 71
  println(solvePart2(input)) // 1149

}
