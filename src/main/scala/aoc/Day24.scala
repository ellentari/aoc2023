package aoc

import aoc.util.{ChineseRemainderTheorem, Coordinate3D, Vector3D}

import scala.collection.mutable

object Day24 extends App {

  case class CRTInput(remainders: Vector[Long], moduli: Vector[Long])

  def solvePart1(vectors: Vector[Vector3D], lowerBound: Long, upperBound: Long): Int = {

    def isWithinBounds(v: Double): Boolean = lowerBound <= v && v <= upperBound

    (for {
      i <- vectors.indices
      j <- i + 1 until vectors.length
      xyAndTimes = vectors(i).findXYIntersectionAndTimes(vectors(j))
    } yield xyAndTimes)
      .count { case (x, y, t1, t2) => t1 >= 0 && t2 >= 0 && isWithinBounds(x) && isWithinBounds(y) }
  }

  def solvePart2(vectors: Vector[Vector3D]): Option[Long] = {
    val maxSum = vectors.map(_.delta.sum.abs).max

    (for {
      velocitiesSum <- -maxSum to maxSum
      crtInput <- buildParametersForCRT(velocitiesSum, vectors)
      coordinatesSum <- ChineseRemainderTheorem.solve(crtInput.remainders, crtInput.moduli)
        if isValidSolution(coordinatesSum, velocitiesSum, vectors)
      _ = println(formatSolution(coordinatesSum, velocitiesSum, vectors))
    } yield coordinatesSum).headOption
  }

  /*
    L0
    x = x0 + dx0 * t
    y = y0 + dy0 * t
    z = z0 + dz0 * t

    L1
    x = x1 + dx1 * s
    y = y1 + dy1 * s
    z = z1 + vz1 * s

    L2
    x = x2 + dx2 * u
    y = y2 + dy2 * u
    z = z2 + dz2 * u

    L3
    x = x3 + dx3 * f
    y = y3 + dy3 * f
    z = z3 + dz3 * f

    When L0 intersects L1 and L0 intersects L2 and L0 intersects L3, then:
    x0 + y0 + z0 + (dx0 + dy0 + dz0) * t1 = x1 + y1 + z1 + (dx1 + dy1 + dz1) * t1 (1)
    x0 + y0 + z0 + (dx0 + dy0 + dz0) * t2 = x2 + y2 + z2 + (dx2 + dy2 + dz2) * t2 (2)
    x0 + y0 + z0 + (dx0 + dy0 + dz0) * t3 = x3 + y3 + z3 + (dx3 + dy3 + dz3) * t3 (3)

    Given:
    a = x0 + y0 + z0
    b = dx0 + dy0 + dz0
    ci = xi + yi + zi
    di = dxi + dyi + dzi

    Then 1-3 equations can be expressed as:
    a + b * ti = ci + di * ti
    a - ci = di * ti - b * ti
    a - ci = ti * (di - b)
    And as a congruence relation:
    a ≡ ci (mod (di - b))

    Chinese remainder theorem:
    a ≡ c1 (mod (d1 - b))
    a ≡ c2 (mod (d2 - b))
    a ≡ c3 (mod (d3 - b))
   */
  private def buildParametersForCRT(fixedVelocitiesSum: Long, vectors: IndexedSeq[Vector3D]): Option[CRTInput] = {
    val cs = mutable.ArrayBuffer.empty[Long]
    val ms = mutable.ArrayBuffer.empty[Long]

    for (i <- vectors.indices) {
      // ci = xi + yi + zi
      val c = vectors(i).start.x + vectors(i).start.y + vectors(i).start.z
      // di = dxi + dyi + dzi
      val d = vectors(i).delta.x + vectors(i).delta.y + vectors(i).delta.z
      val b = fixedVelocitiesSum
      val m = (d - b).abs

      cs.append(c)
      ms.append(m)
    }

    val coprimeIndices = findLargestCoprimeSubset(ms.toVector, maxSize = 6)

    Option.when(coprimeIndices.size >= 3) {
      val coprimeCS = coprimeIndices.map(cs(_)).toVector
      val coprimeMS = coprimeIndices.map(ms(_)).toVector

      CRTInput(coprimeCS, coprimeMS)
    }
  }

  private def findLargestCoprimeSubset(numbers: Vector[Long], maxSize: Int): List[Int] = {
    val coprimeSubset = mutable.ArrayBuffer.empty[Int]
    val factorMap = mutable.Map.empty[Long, Set[Long]]

    for ((number, i) <- numbers.zipWithIndex) {
      val factors = math.primeFactors(number)

      factorMap.update(number, factors)

      if (coprimeSubset.forall(j => factorMap(numbers(j)).intersect(factors).isEmpty))
        coprimeSubset.append(i)

      if (coprimeSubset.size == maxSize)
        return coprimeSubset.toList

    }

    coprimeSubset.toList
  }

  /*
    x0 + y0 + z0 + (vx0 + vy0 + vz0) * t = x1 + y1 + z1 + (vx1 + vy1 + vz1) * t

    a = x0 + y0 + z0
    b = vx0 + vy0 + vz0
    c = x1 + y1 + z1
    d = vx1 + vy1 + vz1

    a + b * t = c + d * t
    b * t - d * t = c - a
    (b - d) * t = c - a
    t = (c - a) / (b - d)
   */
  private def isValidSolution(coordinatesSum: Long, velocitiesSum: Long, vectors: Vector[Vector3D]): Boolean =
    vectors.forall { vector =>
      getCollisionTime(coordinatesSum, velocitiesSum, vector)
        .exists(_ > 0) // time must not be in past for it to be a valid solution
    }

  private def formatSolution(coordinatesSum: Long, velocitySum: Long, vectors: Vector[Vector3D]) = {
    val ts = vectors.map(getCollisionTime(coordinatesSum, velocitySum, _).get)

    vectors.zip(ts)
      .map { case (vec, t) =>
        val collisionAt = vec.atTime(t)

        s"""Hailstone: ${vec.start} @ ${vec.delta}
           |Collision time: $t
           |Collision position: $collisionAt""".stripMargin
      }
      .mkString("\n\n")
  }

  /*
      x0 + y0 + z0 + (vx0 + vy0 + vz0) * t = x1 + y1 + z1 + (vx1 + vy1 + vz1) * t

      a = x0 + y0 + z0
      b = vx0 + vy0 + vz0
      c = x1 + y1 + z1
      d = vx1 + vy1 + vz1

      a + b * t = c + d * t
      b * t - d * t = c - a
      (b - d) * t = c - a
      t = (c - a) / (b - d)

      For a solution to exist:
      b - d must not be 0
      t = (c - a) / (b - d) must be integer
     */
  private def getCollisionTime(coordinatesSum: Long, velocitiesSum: Long, vector: Vector3D): Option[Long] = {
    val a = coordinatesSum
    val b = velocitiesSum

    val c = vector.start.sum
    val d = vector.delta.sum

    Option.when((b - d) != 0 && (c - a) % (b - d) == 0)((c - a) / (b - d))
  }

  private def parse3DVector(s: String): Vector3D = s match {
    case s"$initialPos @ $velocity" =>
      Vector3D(Coordinate3D.parse(initialPos), Coordinate3D.parse(velocity.trim))
  }

  private val sample =
    """19, 13, 30 @ -2,  1, -2
      |18, 19, 22 @ -1, -1, -2
      |20, 25, 34 @ -2, -2, -4
      |12, 31, 28 @ -1, -2, -1
      |20, 19, 15 @  1, -5, -3"""
      .stripMargin
      .split("\n")
      .map(parse3DVector)
      .toVector

  private val input = Input.asList("day24.txt").map(parse3DVector).toVector

  println(solvePart1(sample, 7, 27)) // 2
  println(solvePart1(input, 200000000000000L, 400000000000000L)) // 13754

  println(solvePart2(sample)) // 47
  println(solvePart2(input)) // 711031616315001

}
