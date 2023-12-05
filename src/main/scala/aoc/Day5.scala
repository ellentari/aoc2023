package aoc

import aoc.util.Interval

object Day5 extends App {

  case class Input(seeds: List[Long], mappings: List[RangeMappings])

  case class RangeMappings(value: List[RangeMapping])

  case class RangeMapping(destinationStart: Long, sourceStart: Long, length: Long) {
    def sourceEnd: Long = sourceStart + length - 1
    def sourceInterval: Interval = Interval(sourceStart, sourceEnd)
  }

  def solvePart1(input: Input): Long = {
    val seedIntervals = input.seeds.map(start => Interval(start, start))

    solve(seedIntervals, input.mappings)
  }

  def solvePart2(input: Input): Long = {
    val seedIntervals = input.seeds
      .grouped(2)
      .map { case List(start, length) => Interval.ofLength(start, length) }
      .toList

    solve(seedIntervals, input.mappings)
  }

  private def solve(seeds: List[Interval], mappings: List[RangeMappings]): Long =
    mappings.foldLeft(seeds)((seeds1, mappings) => seeds1.flatMap(mapInterval(mappings)))
      .map(_.start)
      .min

  private def mapInterval(mappings: RangeMappings)(interval: Interval): List[Interval] = {
    val (intersections, mapped) =
      (for {
        mapping <- mappings.value
        intersection <- mapping.sourceInterval.intersect(interval)
        mapped = mapInterval(intersection, mapping)
      } yield (intersection, mapped)).unzip

    val unmapped = interval.removeAll(intersections)

    mapped ::: unmapped
  }

  private def mapInterval(interval: Interval, rangeMapping: RangeMapping): Interval = {
    val offset = interval.start - rangeMapping.sourceStart
    val mappedStart = rangeMapping.destinationStart + offset

    Interval.ofLength(mappedStart, interval.length)
  }

  private def parseInput(input: String): Input = {

    def parseSeeds(line: String) =
      line.split(": ")(1).split("\\s+").map(_.toLong).toList

    def parseRangeMappings(input: String): List[RangeMapping] =
      input.split("\n")
        .drop(1)
        .map(parseRangeMapping)
        .toList
        .sortBy(_.sourceEnd)

    def parseRangeMapping(line: String): RangeMapping = line match {
      case s"$dstStart $srcStart $length" =>
        RangeMapping(dstStart.toLong, srcStart.toLong, length.toLong)
    }

    val parts = input.split("\n\n")
    val seeds = parseSeeds(parts(0))
    val rangeMappings = parts.drop(1).map(parseRangeMappings).map(RangeMappings).toList

    Input(seeds, rangeMappings)
  }

  private val sample = parseInput(
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4""".stripMargin
  )

  private val input1 = parseInput(aoc.Input.asString("day5.txt"))

  println(solvePart1(sample)) // 35
  println(solvePart1(input1)) // 424490994

  println(solvePart2(sample)) // 46
  println(solvePart2(input1)) // 15290096

}
