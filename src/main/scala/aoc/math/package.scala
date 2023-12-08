package aoc

import scala.annotation.tailrec

package object math {

  @tailrec
  def gcd(a: Long, b: Long): Long =
    if (b == 0) a
    else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

}
