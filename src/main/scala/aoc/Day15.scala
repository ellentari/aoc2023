package aoc

import aoc.Day15.HashMap.hash

object Day15 extends App {

  sealed trait Operation
  object Operation {
    case class Remove(key: String) extends Operation
    case class Update(key: String, value: Int) extends Operation
  }

  case class HashMap(underlying: Map[Int, List[(String, Int)]] = Map.empty) {

    def perform(op: Operation): HashMap = {
      val updatedMap = op match {
        case Operation.Remove(key) =>
          val h = hash(key)
          underlying.updatedWith(h)(_.map(_.filterNot(_._1 == key)))

        case Operation.Update(key, value) =>
          val h = hash(key)
          val existing = underlying.getOrElse(h, Nil)

          val upd =
            if (existing.exists(_._1 == key))
              existing.map {
                case (k, _) if k == key => key -> value
                case o => o
              }
            else
              (key -> value) :: existing

          underlying.updated(h, upd)
      }

      HashMap(updatedMap)
    }

    def performAll(ops: Iterable[Operation]): HashMap =
      ops.foldLeft(this)(_.perform(_))

    def power: Int =
      (for {
        (hash, values) <- underlying
        ((_, value), i) <- values.reverse.zipWithIndex
      } yield  (hash + 1) * (i + 1) * value).sum
  }

  object HashMap {
    def hash(s: String): Int =
      s.foldLeft(0)((current, ch) => ((current + ch) * 17) % 256)
  }

  def solvePart1(input: List[String]): Int = input.map(HashMap.hash).sum

  def solvePart2(input: List[Operation]): Int = HashMap().performAll(input).power

  private def parseOperation(s: String): Operation = s match {
    case s"$key=$value" => Operation.Update(key, value.toInt)
    case s"$key-" => Operation.Remove(key)
  }

  private val sample =
    """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7""".split(",").toList

  private val input = Input.asString("day15.txt").split(",").toList

  println(solvePart1(sample)) // 1320
  println(solvePart1(input)) // 514281

  println(solvePart2(sample.map(parseOperation))) // 145
  println(solvePart2(input.map(parseOperation))) // 244199

}
