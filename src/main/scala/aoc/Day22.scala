package aoc

import aoc.util.{Coordinate3D, Interval}

import scala.collection.mutable

object Day22 extends App {

  type BrickLabel = Char

  case class Brick(label: BrickLabel, start: Coordinate3D, end: Coordinate3D) {

    def minZ: Long = start.z min end.z
    def maxZ: Long = start.z max end.z

    def addZ(delta: Long): Brick =
      copy(start = start.copy(z = start.z + delta), end = end.copy(z = end.z + delta))

    private def xsInterval: Interval = Interval(start.x min end.x, start.x max end.x)
    private def ysInterval: Interval = Interval(start.y min end.y, start.y max end.y)

    def intersectsXY(other: Brick): Boolean =
      xsInterval.intersects(other.xsInterval) && ysInterval.intersects(other.ysInterval)
  }

  case class BrickGraph(supports: Map[BrickLabel, IndexedSeq[BrickLabel]]) {

    val supportedBy: Map[BrickLabel, IndexedSeq[BrickLabel]] = supports.toIndexedSeq
      .flatMap { case (support, supported) =>
        supported.map(_ -> support)
      }
      .groupMap(_._1)(_._2)

    def getSupported(brick: BrickLabel): IndexedSeq[BrickLabel] =
      supports.getOrElse(brick, IndexedSeq.empty)

    def isSupportedBy(brick: BrickLabel): IndexedSeq[BrickLabel] =
      supportedBy.getOrElse(brick, IndexedSeq.empty)

  }

  def solvePart1(bricks: Vector[Brick]): Int = {
    val fallen = fall(bricks)
    val graph = buildGraph(fallen)

    def isSafeToRemove(brick: BrickLabel): Boolean =
      graph.getSupported(brick)
        .map(above => graph.isSupportedBy(above).size)
        .forall(_ > 1)

    bricks.map(_.label).count(isSafeToRemove)
  }

  def solvePart2(bricks: Vector[Brick]): Int = {
    val fallen = fall(bricks)
    val graph = buildGraph(fallen)

    def remove(brick: BrickLabel): Int = {
      val indegree = mutable.HashMap.from(graph.supportedBy.view.mapValues(_.length).toMap)

      val queue = scala.collection.mutable.Queue(brick)
      var fallenCount = 0

      while (queue.nonEmpty) {
        val current = queue.dequeue()

        for (above <- graph.getSupported(current)) {
          indegree.update(above, indegree(above) - 1)

          if (indegree(above) == 0) {
            fallenCount += 1
            queue.enqueue(above)
          }
        }
      }

      fallenCount
    }

    bricks.map(_.label).map(remove).sum
  }

  private def fall(bricks: Vector[Brick]): Vector[Brick] = {
    val bricksSorted = bricks.sortBy(_.minZ)
    val result = mutable.ArrayBuffer.empty[Brick]

    for (brick <- bricksSorted) {
      val obstacles = result.filter(_.intersectsXY(brick))
      val firstObstacle = obstacles.map(_.maxZ).maxOption

      val ground = firstObstacle.getOrElse(0L) + 1
      val delta = brick.minZ - ground

      result.append(brick.addZ(-delta))
    }

    result.toVector
  }

  private def buildGraph(bricks: Vector[Brick]): BrickGraph = {

    def supports(support: Brick, supported: Brick): Boolean =
      support.maxZ == supported.minZ - 1 && support.intersectsXY(supported)

    val support = (for {
      i <- bricks.indices
      j <- i + 1 until bricks.length
      support = bricks(i)
      supported = bricks(j) if supports(support, supported)
    } yield (support.label, supported.label))
      .groupMap(_._1)(_._2)

    BrickGraph(support)
  }

  private def parseBrick(s: String, i: Int): Brick = s match {
    case s"$cc1~$cc2" =>
      Brick(('A' + i).toChar, Coordinate3D.parse(cc1), Coordinate3D.parse(cc2))
  }

  private val sample =
    """1,0,1~1,2,1
      |0,0,2~2,0,2
      |0,2,3~2,2,3
      |0,0,4~0,2,4
      |2,0,5~2,2,5
      |0,1,6~2,1,6
      |1,1,8~1,1,9""".stripMargin
      .split("\n")
      .toVector
      .zipWithIndex
      .map((parseBrick _).tupled)

  private val input = Input.asList("day22.txt").zipWithIndex.map((parseBrick _).tupled).toVector

  println(solvePart1(sample)) // 5
  println(solvePart1(input)) // 468

  println(solvePart2(sample)) // 7
  println(solvePart2(input)) // 75358

}
