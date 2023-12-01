package aoc

object Day1 extends App {

  private val Digits = (0 to 9).map(_.toString).toList

  private val WordDigits = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  private val WordDigitsMapping = WordDigits.zipWithIndex.toMap

  def solvePart1(input: List[String]): Int = solve(Digits)(input)

  def solvePart2(input: List[String]): Int = solve(Digits ::: WordDigits)(input)

  private def solve(tokens: List[String])(input: List[String]) =
    input
      .map { line =>
        val (firstToken, _) = tokens.map(t => t -> line.indexOf(t)).filter(_._2 >= 0).minBy(_._2)
        val (lastToken, _) = tokens.map(t => t -> line.lastIndexOf(t)).filter(_._2 >= 0).maxBy(_._2)

        val firstDigit = tokenToDigit(firstToken)
        val secondDigit = tokenToDigit(lastToken)

        firstDigit * 10 + secondDigit
      }
      .sum

  private def tokenToDigit(digitOrWord: String) =
    WordDigitsMapping.get(digitOrWord).fold(Character.digit(digitOrWord(0), 10))(_ + 1)

  private val sample1 =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin.split("\n").toList

  private val sample2 =
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin.split("\n").toList

  private val input = Input.asList("day1.txt")

  println(solvePart1(sample1)) // 142
  println(solvePart1(input)) // 56108

  println(solvePart2(sample2)) // 281
  println(solvePart2(input)) // 55652

}
