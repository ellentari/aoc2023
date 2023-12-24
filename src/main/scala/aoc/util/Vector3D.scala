package aoc.util

case class Vector3D(start: Coordinate3D, delta: Coordinate3D) {

  def atTime(t: Long): Coordinate3D =
    Coordinate3D(start.x + delta.x * t, start.y + delta.y * t, start.z + delta.z * t)

  /*
      L1
      x = x0 + dx0 * t
      y = y0 + dy0 * t
      z = z0 + dz0 * t

      L2
      x = x1 + dx1 * s
      y = y1 + dy1 * s
      z = z1 + dz1 * s

      L1 intersects L2 where:
      x0 + dx0 * t = x1 + dx1 * s (1)
      y0 + dy0 * t = y1 + dy1 * s (2)
      z0 + dz0 * t = z1 + dz1 * s (3)

      Derive t from (1):
      dx0 * t = x1 + dx1 * s - x0
      t = (x1 + dx1 * s - x0) / dx0 (4)

      Derive t from (2):
      dy0 * t = y1 + dy1 * s - y0
      t = (y1 + dy1 * s - y0) / dy0 (5)

      Combine (4) and (5) and solve for s:
      (x1 + dx1 * s - x0) / dx0 = (y1 + dy1 * s - y0) / dy0
      dy0 * (x1 + dx1 * s - x0) = dx0 * (y1 + dy1 * s - y0)
      x1 * dy0 + dx1 * dy0 * s - x0 * dy0 = y1 * dx0 + dy1 * dx0 * s - y0 * dx0
      dx1 * dy0 * s - dy1 * dx0 * s = y1 * dx0 - y0 * dx0 - x1 * dy0 + x0 * dy0
      (dx1 * dy0 - dy1 * dx0) * s = y1 * dx0 - y0 * dx0 - x1 * dy0 + x0 * dy0
      s = (y1 * dx0 - y0 * dx0 - x1 * dy0 + x0 * dy0) / (dx1 * dy0 - dy1 * dx0)

      Then t is either (x1 + dx1 * s - x0) / dx0 or (y1 + dy1 * s - y0) / dy0
     */
  def findXYIntersectionAndTimes(that: Vector3D): (Double, Double, Double, Double) = {
    val x0 = this.start.x
    val dx0 = this.delta.x

    val y0 = this.start.y
    val dy0 = this.delta.y

    val x1 = that.start.x
    val dx1 = that.delta.x

    val y1 = that.start.y
    val dy1 = that.delta.y

    val s = (y1 * dx0 - y0 * dx0 - x1 * dy0 + x0 * dy0).toDouble / (dx1 * dy0 - dy1 * dx0)
    val t = (x1 + dx1 * s - x0) / dx0

    val x = x0 + dx0 * t
    val y = y0 + dy0 * t

    (x, y, t, s)
  }

  /*
    Find intersection (x, y) at times s and t.
    Check t against z0 + dz0 * t = z1 + dz1 * s. If it's true, then x, y, z is the solution.
   */
  def findIntersectionPoint(that: Vector3D): Option[(Double, Double, Double)] = {
    val z0 = this.start.z
    val dz0 = this.delta.z

    val z1 = that.start.z
    val dz1 = that.delta.z

    val (x, y, t, s) = findXYIntersectionAndTimes(that)
    val z = z0 + dz0 * t

    Option.when(z0 + dz0 * t == z1 + dz1 * s)((x, y, z))
  }

}