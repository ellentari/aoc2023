package aoc.util

case class Coordinate3D(x: Int, y: Int, z: Int)

object Coordinate3D {
  def parse(s: String): Coordinate3D = {
    val Array(x, y, z) = s.split(",")
    Coordinate3D(x.toInt, y.toInt, z.toInt)
  }
}
