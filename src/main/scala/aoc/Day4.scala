package aoc

object Day4 extends App {

  case class ScratchCard(winningNumbers: Set[Int], numbersYouHave: Set[Int]) {
    def winCount: Int = numbersYouHave.count(winningNumbers.contains)
  }

  def solvePart1(input: List[ScratchCard]): Int = {
    def points(card: ScratchCard): Int = {
      val winCount = card.winCount
      if (winCount == 0) 0
      else math.pow(2, winCount - 1).toInt
    }

    input.map(points).sum
  }

  def solvePart2(input: List[ScratchCard]): Int = {
    val cardCopies = Array.fill(input.length)(1)

    for ((card, i) <- input.zipWithIndex) {
      val winCount = card.winCount

      for (j <- i + 1 to i + winCount if j < cardCopies.length)
        cardCopies(j) += cardCopies(i)
    }

    cardCopies.sum
  }

  private def parseScratchCard(raw: String): ScratchCard = {

    def parseNumbersSet(raw: String): Set[Int] =
      raw.split("\\s+").map(_.toInt).toSet

    raw match {
      case s"Card $n: $winning | $youHave" =>
        val winningNumbers = parseNumbersSet(winning.trim)
        val numbersYouHave = parseNumbersSet(youHave.trim)

        ScratchCard(winningNumbers, numbersYouHave)
    }
  }

  private val sample =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
      |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
      |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
      |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
      |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
      |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin
      .split("\n")
      .toList
      .map(parseScratchCard)

  private val input = Input.asList("day4.txt").map(parseScratchCard)

  println(solvePart1(sample)) // 13
  println(solvePart1(input)) // 21558

  println(solvePart2(sample)) // 30
  println(solvePart2(input)) // 10425665

}
