package net.andimiller.soon.models

import cats.{Eval, Show}
import cats.implicits.*
import cats.kernel.Semigroup
import cats.parse.{Numbers, Parser}
import net.andimiller.cats.parse.interpolator.*
import net.andimiller.soon.models.TimeUnit.Second
import fansi.Color.{Red}

enum Offset:
  case Single(count: Int, unit: TimeUnit)
  case Add(left: Offset, right: Offset)

  def simplifyCombine: Eval[Map[TimeUnit, Int]] =
    this match
      case Single(count, unit) => Eval.now(Map(unit -> count))
      case Add(left, right)    =>
        (left.simplifyCombine, right.simplifyCombine).mapN { _ |+| _ }

  def iterator: LazyList[Single] =
    this match {
      case s: Offset.Single        =>
        LazyList(s)
      case Offset.Add(left, right) =>
        left.iterator #::: right.iterator
    }

  def granularity: TimeUnit = iterator.filter(_._1 > 0).minBy(_._2)._2

  def toSeconds: Long = this match
    case Offset.Single(count, unit) =>
      (unit.chrono.getDuration.toSeconds * count)
    case Offset.Add(left, right)    =>
      left.toSeconds + right.toSeconds

  def invert: Offset = this match
    case Offset.Single(count, unit) => Offset.Single(0 - count, unit)
    case Offset.Add(left, right)    => Offset.Add(left.invert, right.invert)

  def simplify: Offset = {
    val combined = simplifyCombine.value

    val rebucketed =
      TimeUnit.values.sorted.reverse // we go from the smallest up
        .foldLeft(combined) { case (m, u) =>
          u.nextUnit match {
            case Some(nextUnit) =>
              m.get(u) match {
                case Some(v) =>
                  val (quotient, remainder) = Split(u.maxValue)(v)
                  (m |+| Map(nextUnit -> quotient)).updated(u, remainder)
                case None    => m
              }
            case None           => m
          }
        }
        .filter { case (_, v) => v != 0 }

    TimeUnit.values.sorted
      .flatMap { u =>
        rebucketed.get(u).map(v => Single(v, u))
      }
      .reduceOption(Add.apply)
      .getOrElse(Single(0, Second))
  }

object Offset:
  given Semigroup[Offset]      = Offset.Add(_, _)
  given Show[Offset]           = {
    case Single(count, unit) =>
      if (count <= 0) {
        Red(show"$count$unit").render
      } else {
        show"$count$unit"
      }
    case Add(left, right)    => show"$left $right"
  }
  given parser: Parser[Offset] =
    Parser.recursive[Offset] { recurse =>
      val single = p"${Numbers.signedIntString}${TimeUnit.parser}".map {
        case (d, u) =>
          Single(d.toInt, u)
      }

      p"$single $recurse"
        .map(Semigroup[Offset].combine)
        .backtrack
        .orElse(single)
    }

  def fromString(s: String): Either[String, Offset] =
    parser.parseAll(s).leftMap(_.show)
