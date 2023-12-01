package aoc

import scala.io.Source
import scala.util.Using

object Input {

  def asList(path: String): List[String] =
    Using(Source.fromResource(path))(_.getLines().toList).get

  def asString(path: String): String =
    asList(path).mkString("\n")

}
