package aoc

import aoc.util.Interval
import enumeratum.{Enum, EnumEntry}

import scala.annotation.tailrec

object Day19 extends App {

  sealed trait PartCategory extends EnumEntry with Product with Serializable
  object PartCategory extends Enum[PartCategory] {
    case object X extends PartCategory
    case object M extends PartCategory
    case object A extends PartCategory
    case object S extends PartCategory

    override val values: IndexedSeq[PartCategory] = findValues
  }

  type WorkflowName = String

  case class Workflow(name: WorkflowName, rules: List[Rule])
  object Workflow {
    val AcceptAll: Workflow = Workflow("A", List(Rule.Accept))
    val RejectAll: Workflow = Workflow("R", List(Rule.Reject))

    val Global: List[Workflow] = List(AcceptAll, RejectAll)
  }

  sealed trait Rule
  object Rule {
    case class Conditional(part: PartCategory, condition: Condition, ifTrue: WorkflowName) extends Rule
    case class NextWorkflow(name: WorkflowName) extends Rule
    case object Accept extends Rule
    case object Reject extends Rule
  }

  sealed trait Condition
  object Condition {
    case class LessThan(threshold: Int) extends Condition
    case class GreaterThan(threshold: Int) extends Condition
  }

  import Rule._
  import Condition._

  private val InitialWorkflowName = "in"
  private val MinValue = 1
  private val MaxValue = 4000
  private val FullInterval = Interval(MinValue, MaxValue)

  def solvePart1(workFlows: Map[WorkflowName, Workflow], partRatings: List[Map[PartCategory, Int]]): Int =
    partRatings
      .filter(isPartAccepted(workFlows))
      .map(_.values.sum)
      .sum

  def solvePart2(workflows: Map[WorkflowName, Workflow]): Long =
    countCombinations(workflows)

  private def isPartAccepted(workflows: Map[WorkflowName, Workflow])(partRatings: Map[PartCategory, Int]): Boolean = {

    val allWorkflows = workflows ++ List(
      Workflow.AcceptAll.name -> Workflow.AcceptAll,
      Workflow.RejectAll.name -> Workflow.RejectAll
    )

    def evalWorkflow(workflowName: WorkflowName): Boolean =
      evalRules(allWorkflows(workflowName).rules)

    @tailrec
    def evalRules(rules: List[Rule]): Boolean =
      rules match {
        case rule :: remaining => evalRule(rule) match {
          case None => evalRules(remaining)
          case Some(matchResult) => matchResult match {
            case Left(returnValue) => returnValue
            case Right(nextWorkflow) => evalWorkflow(nextWorkflow)
          }
        }
        case Nil => false
      }

    def evalRule(rule: Rule): Option[Either[Boolean, WorkflowName]] =
      rule match {
        case Conditional(part, condition, ifTrue) =>
          Option.when(evalCondition(condition, partRatings(part)))(Right(ifTrue))
        case NextWorkflow(next) => Some(Right(next))
        case Accept => Some(Left(true))
        case Reject => Some(Left(false))
      }

    def evalCondition(condition: Condition, rating: Int): Boolean =
      condition match {
        case LessThan(threshold) => rating < threshold
        case GreaterThan(threshold) => rating > threshold
      }

    evalWorkflow(InitialWorkflowName)
  }

  private def countCombinations(workflows: Map[WorkflowName, Workflow]): Long = {
    val allWorkflows = workflows ++ List(
      Workflow.AcceptAll.name -> Workflow.AcceptAll,
      Workflow.RejectAll.name -> Workflow.RejectAll
    )

    def toInterval(condition: Condition): Interval = condition match {
      case LessThan(value) => Interval(MinValue, value - 1)
      case GreaterThan(value) => Interval(value + 1, MaxValue)
    }

    def inverse(interval: Interval): Interval = FullInterval.remove(interval).head

    @tailrec
    def foldRules(rules: List[Rule], acc: Long, intervals: Map[PartCategory, Option[Interval]]): Long =
      rules match {
        case Nil => acc
        case rule :: remaining =>
          rule match {
            case Conditional(part, condition, ifTrue) =>
              val interval = toInterval(condition)

              val nextCount = intervals(part)
                .flatMap(_.intersect(interval))
                .fold(0L) { intersection =>
                  loop(ifTrue, intervals.updated(part, Some(intersection)))
                }

              foldRules(remaining, acc + nextCount, intervals
                .updated(part, intervals(part).flatMap(_.intersect(inverse(interval)))))

            case NextWorkflow(next) =>
              val nextCount = loop(next, intervals)

              foldRules(remaining, acc + nextCount, intervals)

            case Accept =>
              foldRules(remaining, acc + intervals.values.map(_.fold(0L)(_.length)).product, intervals)

            case Reject =>
              foldRules(remaining, acc, intervals)
          }
      }

    def loop(current: WorkflowName, intervals: Map[PartCategory, Option[Interval]]): Long = {
      val workflow = allWorkflows(current)

      foldRules(workflow.rules, 0L, intervals)
    }

    loop(InitialWorkflowName, PartCategory.values.map(_ -> Some(FullInterval)).toMap)
  }

  private def parseWorkflowsAndRatings(s: String): (Map[WorkflowName, Workflow], List[Map[PartCategory, Int]]) = {
    val parts = s.split("\n\n")

    val workflows = parts(0).split("\n").map(parseWorkflow).map(wf => wf.name -> wf).toMap
    val ratings = parts(1).split("\n").map(parsePartRatings).toList

    workflows -> ratings
  }

  private def parseWorkflow(line: String): Workflow =
    line match {
      case s"$name{$rulesRaw}" =>
        val rules = rulesRaw.split(",").map(parseRule).toList
        Workflow(name, rules)
    }

  private def parseRule(rule: String): Rule =
    rule match {
      case "A" => Accept
      case "R" => Reject
      case s"$name<$v:$next" =>
        val part = parsePart(name)
        val value = v.toInt

        Conditional(part, LessThan(value), next)
      case s"$name>$v:$next" =>
        val part = parsePart(name)
        val value = v.toInt

        Conditional(part, GreaterThan(value), next)
      case next => NextWorkflow(next)
    }

  private def parsePartRatings(ratings: String): Map[PartCategory, Int] =
    ratings.substring(1, ratings.length - 1)
      .split(",")
      .map { case s"$x=$v" =>
        parsePart(x) -> v.toInt
      }
      .toMap

  private def parsePart(name: String): PartCategory = name match {
    case "x" => PartCategory.X
    case "m" => PartCategory.M
    case "a" => PartCategory.A
    case "s" => PartCategory.S
  }

  private val (sampleWorkflows, sampleRatings) = parseWorkflowsAndRatings(
    """px{a<2006:qkq,m>2090:A,rfg}
      |pv{a>1716:R,A}
      |lnx{m>1548:A,A}
      |rfg{s<537:gd,x>2440:R,A}
      |qs{s>3448:A,lnx}
      |qkq{x<1416:A,crn}
      |crn{x>2662:A,R}
      |in{s<1351:px,qqz}
      |qqz{s>2770:qs,m<1801:hdj,R}
      |gd{a>3333:R,R}
      |hdj{m>838:A,pv}
      |
      |{x=787,m=2655,a=1222,s=2876}
      |{x=1679,m=44,a=2067,s=496}
      |{x=2036,m=264,a=79,s=2244}
      |{x=2461,m=1339,a=466,s=291}
      |{x=2127,m=1623,a=2188,s=1013}""".stripMargin
  )

  private val (inputWorkflows, inputRatings) =
    parseWorkflowsAndRatings(Input.asString("day19.txt"))

  println(solvePart1(sampleWorkflows, sampleRatings)) // 19114
  println(solvePart1(inputWorkflows, inputRatings)) // 472630

  println(solvePart2(sampleWorkflows)) // 167409079868000
  println(solvePart2(inputWorkflows)) // 116738260946855

}
