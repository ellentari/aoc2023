package aoc

import aoc.Day3.EngineSchemaPart.{Number, Symbol}
import aoc.util.Grid

import scala.collection.mutable

object Day3 extends App {

  case class EngineSchema(graph: Map[IdEngineSchemaPart, List[IdEngineSchemaPart]]) {
    def partNumbers: List[Number] =
      graph
        .collect {
          case (IdEngineSchemaPart(_, num: Number), adjacent) if adjacent.exists(_.part.isSymbol) =>
            num
        }
        .toList

    def gears: List[Gear] =
      graph
        .collect {
          case (part, List(IdEngineSchemaPart(_, n1: Number), IdEngineSchemaPart(_, n2: Number))) if part.part.isGear =>
            Gear(n1, n2)
        }
        .toList
  }

  case class IdEngineSchemaPart(id: Int, part: EngineSchemaPart)

  sealed trait EngineSchemaPart extends Product with Serializable {
    def isSymbol: Boolean
    def isGear: Boolean
  }

  object EngineSchemaPart {
    case class Number(value: Int) extends EngineSchemaPart {
      def isSymbol: Boolean = false
      def isGear: Boolean = false
    }

    case class Symbol(value: Char) extends EngineSchemaPart {
      def isSymbol: Boolean = true
      def isGear: Boolean = value == '*'
    }
  }

  case class Gear(number1: Number, number2: Number) {
    def gearRatio: Int = number1.value * number2.value
  }

  def solvePart1(schema: EngineSchema): Int =
    schema.partNumbers.map(_.value).sum

  def solvePart2(schema: EngineSchema): Int =
    schema.gears.map(_.gearRatio).sum

  private def parseEngineSchema(input: String): EngineSchema =
    EngineSchema(parseGraph(Grid.parseCharacterGrid(input)))

  private def parseGraph(grid: Grid[Char]): Map[IdEngineSchemaPart, List[IdEngineSchemaPart]] = {
    val seen = Array.ofDim[Boolean](grid.height, grid.width)
    val result = mutable.HashMap.empty[IdEngineSchemaPart, List[IdEngineSchemaPart]]
    var id = 1

    def shouldVisit(index: Grid.Index): Boolean =
      !seen(index.row)(index.column) && grid(index) != '.'

    def discoverNumber(index: Grid.Index): (Number, Int, Int) = {
      var start = index.column
      while (start > 0 && grid(index.row, start - 1).isDigit)
        start -= 1

      var end = index.column
      while (end < grid.rows(index.row).size - 1 && grid(index.row, end + 1).isDigit)
        end += 1

      var number = 0

      for (col <- start to end) {
        number *= 10
        number += Character.digit(grid(index.row, col), 10)
      }

      (Number(number), start, end)
    }

    def discoverSymbol(index: Grid.Index): (Symbol, Int, Int) =
      (Symbol(grid(index)), index.column, index.column)

    def discover(index: Grid.Index): IdEngineSchemaPart = {
      val (part, start, end) =
        if (grid(index).isDigit) discoverNumber(index)
        else discoverSymbol(index)

      for (col <- start to end)
        seen(index.row)(col) = true

      val idPart = IdEngineSchemaPart(id, part)

      id += 1

      for {
        col <- start to end
        next <- grid.adjacent8(index.row, col) if shouldVisit(next)
      } {
        val nextPart = discover(next)

        result.updateWith(nextPart)(adjacent => Some(idPart :: adjacent.getOrElse(Nil)))
        result.updateWith(idPart)(adjacent => Some(nextPart :: adjacent.getOrElse(Nil)))
      }

      idPart
    }

    for (index <- grid.indices if shouldVisit(index))
      discover(index)

    result.toMap
  }

  private val sample = parseEngineSchema(
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin
  )

  private val input = parseEngineSchema(Input.asString("day3.txt"))

  println(solvePart1(sample)) // 4361
  println(solvePart1(input)) // 537832

  println(solvePart2(sample)) // 467835
  println(solvePart2(input)) // 81939900

}
