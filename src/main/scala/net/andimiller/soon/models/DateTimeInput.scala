package net.andimiller.soon.models

import cats.data.{NonEmptyList, ValidatedNel}
import cats.implicits.*
import cats.parse.{Numbers, Parser, Parser0}
import com.monovore.decline.Argument

import java.time.{LocalDate, LocalDateTime}

enum DateTimeInput:
  case Relative(offset: Offset)
  case AbsoluteDate(date: LocalDate)
  case AbsoluteDateTime(dateTime: LocalDateTime, granularity: TimeUnit)

object DateTimeInput:

  private val digit4: Parser[String] =
    Numbers.digit.rep(4, 4).map(_.toList.mkString)
  private val digit2: Parser[String] =
    Numbers.digit.rep(2, 2).map(_.toList.mkString)
  private val dash: Parser[Unit]     = Parser.char('-')
  private val colon: Parser[Unit]    = Parser.char(':')
  private val timeT: Parser[Unit]    = Parser.char('T')

  private val dateParser: Parser[LocalDate] =
    (digit4 <* dash, digit2 <* dash, digit2).mapN { (y, m, d) =>
      LocalDate.of(y.toInt, m.toInt, d.toInt)
    }

  private val timeHMS: Parser[(String, String, String)] =
    (digit2 <* colon, digit2 <* colon, digit2).tupled

  private val timeHM: Parser[(String, String)] =
    (digit2 <* colon, digit2).tupled

  private val dateTimeParser: Parser[(LocalDateTime, TimeUnit)] =
    (dateParser <* timeT).flatMap { date =>
      timeHMS
        .map { case (h, m, s) =>
          (
            LocalDateTime.of(
              date.getYear,
              date.getMonthValue,
              date.getDayOfMonth,
              h.toInt,
              m.toInt,
              s.toInt
            ),
            TimeUnit.Second
          )
        }
        .backtrack
        .orElse(
          timeHM.map { case (h, m) =>
            (
              LocalDateTime.of(
                date.getYear,
                date.getMonthValue,
                date.getDayOfMonth,
                h.toInt,
                m.toInt
              ),
              TimeUnit.Minute
            )
          }
        )
    }

  given parser: Parser[DateTimeInput] =
    dateTimeParser
      .map { case (dt, g) => AbsoluteDateTime(dt, g) }
      .backtrack
      .orElse(
        dateParser.map(AbsoluteDate(_))
      )
      .backtrack
      .orElse(
        Offset.parser.map(Relative(_))
      )

  def fromString(s: String): Either[String, DateTimeInput] =
    parser.parseAll(s).leftMap(_.show)

  given Argument[DateTimeInput] = new Argument[DateTimeInput]:
    override def read(string: String): ValidatedNel[String, DateTimeInput] =
      fromString(string).toValidatedNel

    override def defaultMetavar: String = "3d 10h | 2026-03-15"
