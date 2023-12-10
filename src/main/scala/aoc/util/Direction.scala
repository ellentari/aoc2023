package aoc.util

sealed trait Direction {
  def opposite: Direction
}

object Direction {
  object South extends Direction {
    def opposite: Direction = North
  }

  object North extends Direction {
    def opposite: Direction = South
  }

  object East extends Direction {
    def opposite: Direction = West
  }

  object West extends Direction {
    def opposite: Direction = East
  }
}
