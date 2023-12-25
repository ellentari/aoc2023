package aoc.algo

class DisjointSet(size: Int) {

  private val parent = Array.fill[Int](size)(-1)

  def find(v: Int): Int =
    if (parent(v) < 0) v
    else {
      val p = find(parent(v))
      parent(v) = p
      p
    }

  def union(v1: Int, v2: Int): Boolean = {
    val p1 = find(v1)
    val p2 = find(v2)

    if (p1 != p2) {
      val sizeP1 = parent(p1).abs
      val sizeP2 = parent(p2).abs

      if (sizeP1 >= sizeP2) {
        parent(p2) = p1
        parent(p1) -= sizeP2
      } else {
        parent(p1) = p2
        parent(p2) -= sizeP1
      }

      true
    } else
      false
  }

}
