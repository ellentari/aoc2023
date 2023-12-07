package aoc

import enumeratum.{Enum, EnumEntry}

object Day7 extends App {

  case class HandWithBid(hand: Hand, bid: Int)

  case class Hand(cards: Vector[Card]) {
    require(cards.length == 5, "Hand must have exactly 5 cards")

    def cardCount: Map[Card, Int] =
      cards.groupBy(identity).view.mapValues(_.size).toMap

    override def toString: String = cards.map(_.asChar).mkString
  }

  sealed abstract class Card(val asChar: Char) extends EnumEntry

  object Card extends Enum[Card] {
    val values: IndexedSeq[Card] = findValues

    case object Ace extends Card('A')
    case object King extends Card('K')
    case object Queen extends Card('Q')
    case object Joker extends Card('J')
    case object Ten extends Card('T')
    case object Nine extends Card('9')
    case object Eight extends Card('8')
    case object Seven extends Card('7')
    case object Six extends Card('6')
    case object Five extends Card('5')
    case object Four extends Card('4')
    case object Three extends Card('3')
    case object Two extends Card('2')

    def fromChar(char: Char): Option[Card] =
      values.find(_.asChar == char)
  }

  import Card._

  sealed abstract class HandType(val strength: Int) extends EnumEntry

  object HandType extends Enum[HandType] {
    val values: IndexedSeq[HandType] = findValues
    private val valuesSortedByStrengthDesc: IndexedSeq[HandType] = values.sortBy(-_.strength)

    case object HighCard extends HandType(1)
    case object OnePair extends HandType(2)
    case object TwoPair extends HandType(3)
    case object ThreeOfAKind extends HandType(4)
    case object FullHouse extends HandType(5)
    case object FourOfAKind extends HandType(6)
    case object FiveOfAKind extends HandType(7)

    def of(hand: Hand): HandType = fromCardCount(hand.cardCount)

    def jokerRule(hand: Hand): HandType = {
      val cardCount = hand.cardCount
      val jokerCount = cardCount.getOrElse(Joker, 0)

      if (jokerCount == 0 || jokerCount == hand.cards.size)
        fromCardCount(cardCount)
      else {
        val cardCountWoJoker = cardCount - Joker
        val (mostFrequentCard, mostFrequentCardCount) = cardCountWoJoker.maxBy(_._2)

        val updCardCount = cardCountWoJoker
          .updated(mostFrequentCard, mostFrequentCardCount + jokerCount)

        fromCardCount(updCardCount)
      }
    }

    private def fromCardCount(cardCount: Map[Card, Int]): HandType = {
      val cardCountsSorted = cardCount.values.toList.sorted

      def matchesHand(handType: HandType): Boolean = handType match {
        case HighCard => cardCountsSorted.size == 5
        case OnePair => cardCountsSorted.contains(2)
        case TwoPair => cardCountsSorted == List(1, 2, 2)
        case ThreeOfAKind => cardCountsSorted.contains(3)
        case FullHouse => cardCountsSorted == List(2, 3)
        case FourOfAKind => cardCountsSorted == List(1, 4)
        case FiveOfAKind => cardCountsSorted.size == 1
      }

      valuesSortedByStrengthDesc.find(matchesHand).get
    }
  }

  object HandOrderRules {
    val part1: Ordering[Hand] = handOrdering(
      HandType.of,
      List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Joker, Queen, King, Ace))

    val part2: Ordering[Hand] = handOrdering(
      HandType.jokerRule,
      List(Joker, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Queen, King, Ace))

    private def handOrdering(handTypeOf: Hand => HandType, orderedCards: List[Card]): Ordering[Hand] = {
      val cardOrderOf = orderedCards.zipWithIndex.toMap

      val byHandType: Ordering[Hand] = Ordering.by(handTypeOf(_).strength)
      val byStrongestCard: Ordering[Hand] = new Ordering[Hand] {
        def compare(x: Hand, y: Hand): Int = {
          for (i <- x.cards.indices) {
            val xCardOrder = cardOrderOf(x.cards(i))
            val yCardOrder = cardOrderOf(y.cards(i))

            val result = Ordering[Int].compare(xCardOrder, yCardOrder)

            if (result != 0)
              return result
          }

          0
        }
      }

      byHandType.orElse(byStrongestCard)
    }

  }

  def solvePart1(input: List[HandWithBid]): Long = solve(input, HandOrderRules.part1)

  def solvePart2(input: List[HandWithBid]): Long = solve(input, HandOrderRules.part2)

  private def solve(input: List[HandWithBid], ordering: Ordering[Hand]): Long =
    input
      .sortBy(_.hand)(ordering)
      .zipWithIndex
      .map { case (card, i) =>
        val rank = i + 1
        card.bid.toLong * rank
      }
      .sum

  private def parseHandWithBid(input: String): HandWithBid = {

    def parseHand(raw: String): Hand = {
      val cards = raw.flatMap(Card.fromChar).toVector
      Hand(cards)
    }

    val parts = input.split(" ")
    val hand = parseHand(parts(0))
    val bid = parts(1).toInt

    HandWithBid(hand, bid)
  }

  private val sample =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin
      .split("\n")
      .toList
      .map(parseHandWithBid)

  private val input = Input.asList("day7.txt").map(parseHandWithBid)

  println(solvePart1(sample)) // 6440
  println(solvePart1(input)) // 250453939

  println(solvePart2(sample)) // 5905
  println(solvePart2(input)) // 248652697

}
