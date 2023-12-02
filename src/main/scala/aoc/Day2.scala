package aoc

import enumeratum.{Enum, EnumEntry}

object Day2 extends App {

  case class Game(id: Int, cubeSets: List[CubeSet]) {

    def isPossible(target: CubeSet): Boolean =
      cubeSets.forall(target.contains)

    def minCubeSet: CubeSet =
      CubeSet(
        CubeColor.values
          .map(color => color -> maxCount(color))
          .toMap
      )

    private def maxCount(color: CubeColor): Int =
      cubeSets
        .map(_.cubeCounts.getOrElse(color, 0))
        .maxOption
        .getOrElse(0)
  }

  case class CubeSet(cubeCounts: Map[CubeColor, Int]) {

    def contains(other: CubeSet): Boolean =
      other.cubeCounts.forall { case (color, count) =>
        cubeCounts.get(color).exists(count <= _)
      }

    def power: Int = cubeCounts.values.product

  }

  sealed trait CubeColor extends EnumEntry with Product with Serializable

  object CubeColor extends Enum[CubeColor] {
    val values: IndexedSeq[CubeColor] = findValues

    case object Red extends CubeColor
    case object Green extends CubeColor
    case object Blue extends CubeColor
  }

  def solvePart1(games: List[Game]): Int =
    games
      .filter(_.isPossible(Part1GameConfiguration))
      .map(_.id)
      .sum

  def solvePart2(games: List[Game]): Int =
    games
      .map(_.minCubeSet)
      .map(_.power)
      .sum

  private def parseGame(rawInput: String): Game = {

    def parseCubeSet(raw: String): CubeSet = {
      val cubeCounts = raw.split(", ").map(parseCubeCount).toMap

      CubeSet(cubeCounts)
    }

    def parseCubeCount(raw: String) = raw match {
      case s"$countRaw $colorRaw" =>
        val count = countRaw.toInt
        val cubeColor = parseCubeColor(colorRaw)

        cubeColor -> count
    }

    def parseCubeColor(raw: String): CubeColor = raw match {
      case "red" => CubeColor.Red
      case "green" => CubeColor.Green
      case "blue" => CubeColor.Blue
    }

    rawInput match {
      case s"Game $gameIdRaw: $cubeSetsRaw" =>
        val gameId = gameIdRaw.toInt
        val cubeSets = cubeSetsRaw.split("; ").map(parseCubeSet).toList

        Game(gameId, cubeSets)
    }
  }

  private val sample =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin
      .split("\n")
      .toList
      .map(parseGame)

  private val input = Input.asList("day2.txt").map(parseGame)

  private val Part1GameConfiguration = CubeSet(Map(
    CubeColor.Red -> 12,
    CubeColor.Green -> 13,
    CubeColor.Blue -> 14
  ))

  println(solvePart1(sample)) // 8
  println(solvePart1(input)) // 2913

  println(solvePart2(sample)) // 2286
  println(solvePart2(input)) // 55593

}
