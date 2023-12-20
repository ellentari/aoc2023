package aoc


import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day20 extends App {

  private val BroadcasterName = "broadcaster"

  type ModuleName = String

  case class ModuleGraph(
    modules: Map[ModuleName, Module],
    outputs: Map[ModuleName, List[ModuleName]],
    inputs: Map[ModuleName, List[ModuleName]])

  sealed trait Module
  object Module {
    case class FlipFlop(on: Boolean = false) extends Module
    case class Conjunction(inputs: Map[ModuleName, Pulse] = Map.empty) extends Module
    case object Broadcast extends Module
    case object Sink extends Module
  }

  sealed trait Pulse
  object Pulse {
    case object High extends Pulse
    case object Low extends Pulse
  }

  import Module._
  import Pulse._

  case class State(modules: Map[ModuleName, Module], pulseCount: Map[Pulse, Int] = Map.empty) {

    def updateModules(newModules: Map[ModuleName, Module]): State =
      copy(modules = newModules)

    def addPulse(pulse: Pulse): State =
      copy(pulseCount = pulseCount.updatedWith(pulse)(c => Some(c.getOrElse(0) + 1)))
  }

  def solvePart1(graph: ModuleGraph): Long = {
    val finalState = (0 until 1000)
      .foldLeft(State(graph.modules))((state, _) =>
        evolveState(state, graph)
      )

    finalState.pulseCount
      .values
      .map(_.toLong)
      .product
  }

  private def evolveState(initial: State, graph: ModuleGraph): State = {

    def receivePulse(pulse: Pulse, from: ModuleName, to: ModuleName, modules: Map[ModuleName, Module]): Option[(Pulse, Module)] =
      modules(to) match {
        case FlipFlop(on) =>
          pulse match {
            case High => None
            case Low =>
              val updModule = FlipFlop(!on)
              val nextPulse = if (on) Low else High
              Some((nextPulse, updModule))
          }

        case Module.Conjunction(inputs) =>
          val updModule = Module.Conjunction(inputs.updated(from, pulse))
          val allHigh = graph.inputs.getOrElse(to, Nil).map(updModule.inputs.getOrElse(_, Low)).forall(_ == High)
          val nextPulse = if (allHigh) Low else High
          Some((nextPulse, updModule))

        case Module.Broadcast =>
          Some((pulse, Module.Broadcast))

        case Module.Sink => None
      }

    @tailrec
    def loop(queue: Queue[(ModuleName, Pulse, ModuleName)], state: State): State =
      queue.dequeueOption match {
        case None => state
        case Some(((from, pulse, to), tail)) =>
          receivePulse(pulse, from, to, state.modules) match {
            case None =>
              loop(tail, state.addPulse(pulse))

            case Some((nextPulse, updModule)) =>
              val toProcess = graph.outputs.getOrElse(to, Nil).map(next => (to, nextPulse, next))
              val nextState = state
                .updateModules(state.modules.updated(to, updModule))
                .addPulse(pulse)

              loop(tail ++ toProcess, nextState)
          }
      }

    loop(Queue(("button", Low, BroadcasterName)), initial)
  }

  private def parseModuleGraph(input: List[String]): ModuleGraph = {

    def split(s: String): List[String] = s.split(", ").toList

    val tuples = input
      .map {
        case s"$source -> $outputs" =>
          val outputsList = split(outputs)

          val (moduleName, module) = source match {
            case s"%$a" => a -> FlipFlop()
            case s"&$a" => a -> Conjunction()
            case BroadcasterName => BroadcasterName -> Broadcast
          }

          (moduleName, module, outputsList)
      }

    val outputs = tuples.map { case (name, _, outputs) => name -> outputs }.toMap
    val inputs = outputs.toList.flatMap { case (input, outputs) => outputs.map(_ -> input) }.groupMap(_._1)(_._2)
    val modules = tuples
      .map { case (name, module, _) => name -> module }
      .toMap

    val outputModules = outputs.values.flatten.filterNot(modules.contains).map(_ -> Sink).toMap

    ModuleGraph(modules ++ outputModules, outputs, inputs)
  }

  private val sample = parseModuleGraph(
    """broadcaster -> a, b, c
      |%a -> b
      |%b -> c
      |%c -> inv
      |&inv -> a""".stripMargin
      .split("\n")
      .toList
  )

  private val sample2 = parseModuleGraph(
    """broadcaster -> a
      |%a -> inv, con
      |&inv -> b
      |%b -> con
      |&con -> output""".stripMargin
      .split("\n")
      .toList
  )

  private val input = parseModuleGraph(Input.asList("day20.txt"))

  println(solvePart1(sample)) // 32000000
  println(solvePart1(sample2)) // 11687500
  println(solvePart1(input)) // 763500168

}
