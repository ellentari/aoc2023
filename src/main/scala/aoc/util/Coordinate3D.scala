package aoc.util

case class Coordinate3D(x: Long, y: Long, z: Long) {
  def sum: Long = x + y + z
  override def toString: String = s"$x, $y, $z"
}

object Coordinate3D {
  def parse(s: String): Coordinate3D = {
    val Array(x, y, z) = s.split(",\\s+")
    Coordinate3D(x.toLong, y.toLong, z.toLong)
  }
}
