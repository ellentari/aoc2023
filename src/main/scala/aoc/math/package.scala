package aoc

import scala.annotation.tailrec
import scala.collection.mutable

package object math {

  @tailrec
  def gcd(a: Long, b: Long): Long =
    if (b == 0) a
    else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

  def extendedGCD(a: Long, b: Long): (Long, Long, Long) =
    if (b == 0) (a, 1, 0)
    else {
      val (gcd, x1, y1) = extendedGCD(b, a % b)

      val x = y1
      val y = x1 - (a / b) * y1

      (gcd, x, y)
    }

  def multiplicativeInverse(value: Long, mod: Long): Long = {
    val (_, x, _) = extendedGCD(value, mod)

    (x % mod + mod) % mod
  }

  def primeFactors(number: Long): Set[Long] = {
    if (number <= 0)
      return Set.empty

    var n = number
    val factors = mutable.HashSet.empty[Long]

    // Divide by 2 while even
    while (n % 2 == 0) {
      factors.add(2)
      n /= 2
    }

    // Divide by odd numbers from 3 to sqrt(number)
    for (i <- 3 to scala.math.sqrt(n).toInt by 2) {
      while (n % i == 0) {
        factors.add(i)
        n /= i
      }
    }

    // If number is a prime greater than 2
    if (n > 2) {
      factors.add(n)
    }

    factors.toSet
  }

}
