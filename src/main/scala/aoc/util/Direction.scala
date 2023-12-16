package aoc.util

sealed trait Direction {
  def opposite: Direction
  def turnLeft: Direction
  def turnRight: Direction
  override def toString: String = getClass.getSimpleName
}

object Direction {
  object South extends Direction {
    def opposite: Direction = North
    def turnLeft: Direction = East
    def turnRight: Direction = West
  }

  object North extends Direction {
    def opposite: Direction = South
    def turnLeft: Direction = West
    def turnRight: Direction = East
  }

  object East extends Direction {
    def opposite: Direction = West
    def turnLeft: Direction = North
    def turnRight: Direction = South
  }

  object West extends Direction {
    def opposite: Direction = East
    def turnLeft: Direction = South
    def turnRight: Direction = North
  }
}
