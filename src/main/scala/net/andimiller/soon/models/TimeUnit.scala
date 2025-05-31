package net.andimiller.soon.models

import cats.Show
import cats.parse.Parser
import net.andimiller.cats.parse.interpolator.*
import cats.implicits.*
import io.circe.{Decoder, Encoder}
import scala.concurrent.duration

import java.time.temporal.ChronoUnit

enum TimeUnit:
  case Year, Day, Hour, Minute, Second

  def chrono: ChronoUnit = this match
    case Year   => ChronoUnit.YEARS
    case Day    => ChronoUnit.DAYS
    case Hour   => ChronoUnit.HOURS
    case Minute => ChronoUnit.MINUTES
    case Second => ChronoUnit.SECONDS

  def maxValue: Int = this match
    case Year   => 1000
    case Day    => 365
    case Hour   => 24
    case Minute => 60
    case Second => 60

  def nextUnit: Option[TimeUnit] = this match
    case Year   => None
    case Day    => Some(Year)
    case Hour   => Some(Day)
    case Minute => Some(Hour)
    case Second => Some(Minute)

object TimeUnit:
  given Ordering[TimeUnit] = Ordering.by {
    case Year   => 0
    case Day    => 1
    case Hour   => 2
    case Minute => 3
    case Second => 4
  }

  given Show[TimeUnit] = Show.show {
    case Year   => "y"
    case Day    => "d"
    case Hour   => "h"
    case Minute => "m"
    case Second => "s"
  }

  given parser: Parser[TimeUnit] =
    p"y".as(Year) <+> p"d".as(Day) <+> p"h".as(Hour) <+> p"m".as(
      Minute
    ) <+> p"s".as(Second)

  def fromString(s: String): Either[String, TimeUnit] =
    parser.parseAll(s).leftMap(_.show)

  given Encoder[TimeUnit] = Encoder[String].contramap(_.show)
  given Decoder[TimeUnit] = Decoder[String].emap(fromString)
