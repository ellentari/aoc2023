package aoc

import scala.annotation.tailrec

object Day8 extends App {

  case class Input(instructions: Vector[Instruction], network: Network)

  sealed trait Instruction
  object Instruction {
    case object Left extends Instruction
    case object Right extends Instruction
  }

  case class Network(underlying: Map[String, (String, String)]) {
    def getNext(from: String, instruction: Instruction): String = {
      val (left, right) = underlying(from)

      instruction match {
        case Instruction.Left => left
        case Instruction.Right => right
      }
    }
  }

  def solvePart1(input: Input): Int =
    pathLength(input, _ == "ZZZ")("AAA")

  def solvePart2(input: Input): Long =
    input.network.underlying.keys
      .filter(_.endsWith("A"))
      .map(pathLength(input, _.endsWith("Z")))
      .map(_.toLong)
      .reduce(math.lcm)

  private def pathLength(input: Input, isEnd: String => Boolean)(start: String): Int = {
    @tailrec
    def loop(current: String, steps: Int): Int =
      if (isEnd(current)) steps
      else {
        val instruction = input.instructions(steps % input.instructions.length)
        val next = input.network.getNext(current, instruction)

        loop(next, steps + 1)
      }

    loop(start, 0)
  }

  private def parseInput(input: List[String]): Input = {

    def parseInstruction(char: Char): Instruction = char match {
      case 'R' => Instruction.Right
      case 'L' => Instruction.Left
    }

    def parseNetworkLine(line: String): (String, (String, String)) = line match {
      case s"$from = ($left, $right)" => (from, (left, right))
    }

    val instructions = input.head.map(parseInstruction).toVector
    val network = input.drop(2).map(parseNetworkLine).toMap

    Input(instructions, Network(network))
  }

  private val sample1 = parseInput(
    """RL
      |
      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin.split("\n").toList
  )

  private val sample2 = parseInput(
    """LR
      |
      |11A = (11B, XXX)
      |11B = (XXX, 11Z)
      |11Z = (11B, XXX)
      |22A = (22B, XXX)
      |22B = (22C, 22C)
      |22C = (22Z, 22Z)
      |22Z = (22B, 22B)
      |XXX = (XXX, XXX)""".stripMargin.split("\n").toList
  )

  private val input = parseInput(aoc.Input.asList("day8.txt"))

  println(solvePart1(sample1)) // 2
  println(solvePart1(input)) // 11567

  println(solvePart2(sample2)) // 6
  println(solvePart2(input)) // 9858474970153

}
