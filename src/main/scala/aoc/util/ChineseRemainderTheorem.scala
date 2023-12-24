package aoc.util

import scala.util.Try

object ChineseRemainderTheorem {

  def solve(remainders: Vector[Long], moduli: Vector[Long]): Option[Long] =
    Try(doSolve(remainders, moduli)).toOption

  /*
      X ≡ a1 (mod m1)
      X ≡ a2 (mod m2)
      X ≡ a3 (mod m3)

      X = (a1 * M1 * Minv_1 + a2 * M2 * Minv_2 + ... + an * Mn * Minv_n) % M

      M = m1 * m2 * ... * mn
      Mi = M / mi
      Mi * Minv_i ≡ 1 mod mi (multiplicative inverse)
   */
  private def doSolve(remainders: Vector[Long], moduli: Vector[Long]): Long = {
    val M = moduli.product

    if (moduli.map(BigDecimal(_)).product != BigDecimal(M))
      throw new RuntimeException("Long overflow")

    val Mi = Array.ofDim[Long](remainders.length)
    val Minv = Array.ofDim[Long](remainders.length)

    for (i <- moduli.indices)
      Mi(i) = M / moduli(i)

    for (i <- moduli.indices)
      Minv(i) = aoc.math.multiplicativeInverse(Mi(i), moduli(i))

    var X = BigDecimal(0)

    for (i <- remainders.indices) {
      X += BigDecimal.decimal(remainders(i)) *  Mi(i) * Minv(i)
      X %= M
    }

    X.toLong
  }

}
