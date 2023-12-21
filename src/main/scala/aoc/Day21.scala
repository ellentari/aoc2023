package aoc

import aoc.algo.BFS
import aoc.util.Grid

import scala.math.pow

object Day21 extends App {

  def solvePart1(grid: Grid[Char], maxDistance: Int): Int = {
    val start = grid.indexOf(_ == 'S').get
    var result = 0

    BFS.visitAllWithDistance(List(start))(
      (idx, distance) =>
        if (distance < maxDistance)
          grid
            .adjacent4(idx)
            .filter(grid(_) == '.')
        else
          Nil,
      (_, distance) =>
        if (distance % 2 == maxDistance % 2)
          result += 1
    )

    result
  }

  // total = (n + 1)^2 * odd full + n^2 * even full - (n + 1) * odd corners + n * even corners
  def solvePart2(grid: Grid[Char], maxDistance: Int): Long = {
    val start = grid.indexOf(_ == 'S').get
    val allDistances = scala.collection.mutable.ListBuffer.empty[Int]

    BFS.visitAllWithDistance(List(start))(
      (state, _) => grid.adjacent4(state).filter(grid(_) == '.'),
      (_, distance) => allDistances.addOne(distance)
    )

    val n = maxDistance / grid.width
    val distanceTillEdge = maxDistance % grid.width

    val oddDistances = allDistances.filter(_ % 2 != 0)
    val evenDistances = allDistances.filter(_ % 2 == 0)

    val oddFull = oddDistances.size
    val evenFull = evenDistances.size

    val oddCorners = oddDistances.count(_ > distanceTillEdge)
    val evenCorners = evenDistances.count(_ > distanceTillEdge)

    pow(n + 1, 2).toLong * oddFull + pow(n, 2).toLong * evenFull - (n + 1) * oddCorners + n * evenCorners
  }

  private val sample = Grid.parseCharacterGrid(
    """...........
      |.....###.#.
      |.###.##..#.
      |..#.#...#..
      |....#.#....
      |.##..S####.
      |.##..#...#.
      |.......##..
      |.##.#.####.
      |.##..##.##.
      |...........""".stripMargin)

  private val input = Grid.parseCharacterGrid(Input.asString("day21.txt"))

  println(solvePart1(sample, 6)) // 16
  println(solvePart1(input, 64)) // 3830

  println(solvePart2(input, 26501365)) // 637087163925555

}
