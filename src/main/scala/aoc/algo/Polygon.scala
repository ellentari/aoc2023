package aoc.algo

import aoc.util.{Coordinate2D, Direction}

object Polygon extends App {

  // Pick's theorem A = I + B/2 - 1.
  // I = A - B/2 + 1
  // I + B = A + B/2 + 1
  def areaIncludingBoundaries(coordinates: Vector[Coordinate2D]): Long = {
    require(coordinates.size >= 3, "Vector size must be at least 3")

    // close the polygon
    val coordinates1 = coordinates :+ coordinates.head

    val area = shoelaceArea(coordinates1)
    val p = perimeter(coordinates1)

    area + p / 2 + 1
  }

  def shoelaceArea(coordinates: Vector[Coordinate2D]): Long = {
    require(coordinates.size >= 3, "Vector size must be at least 3")

    // close the polygon
    val coordinates1 = coordinates :+ coordinates.head

    calculateShoelaceArea(coordinates1)
  }

  // K = 1/2 * |(x1 * y2 – x2 * y1) + (x2 * y3 - x3 * y2) +...+ (xn-1 * yn - xn * yn−1) + (xn * y1 – x1 * yn)|
  // assumes, that the last term of coordinates is it's first term
  private def calculateShoelaceArea(coordinates: Vector[Coordinate2D]): Long =
    (1 until coordinates.length)
      .map { n =>
        val xn_1 = coordinates(n - 1).x.toLong
        val yn_1 = coordinates(n - 1).y.toLong

        val xn = coordinates(n).x.toLong
        val yn = coordinates(n).y.toLong

        xn_1 * yn - xn * yn_1
      }
      .sum
      .abs / 2

  private def perimeter(coordinates: Vector[Coordinate2D]): Long =
    (1 until coordinates.length)
      .map { n =>
        val xn_1 = coordinates(n - 1).x.toLong
        val yn_1 = coordinates(n - 1).y.toLong

        val xn = coordinates(n).x.toLong
        val yn = coordinates(n).y.toLong

        (xn - xn_1).abs + (yn - yn_1).abs
      }
      .sum


}
