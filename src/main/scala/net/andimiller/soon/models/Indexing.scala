package net.andimiller.soon.models

import cats.data.{NonEmptyList, ValidatedNel}
import com.monovore.decline.Argument
import io.circe.Codec
import cats.implicits.*

import scala.util.Try

object Indexing:

  enum Mode derives Codec:
    case alpha, alphanum
    case number, hex, octal

  object Mode:
    given Argument[Mode] = new Argument[Mode]:
      override def read(string: String): ValidatedNel[String, Mode] =
        Try {
          Mode.valueOf(string)
        }.toEither.toValidated.leftMap(e => NonEmptyList.of(e.getMessage))
      override def defaultMetavar: String                           = "alpha"

  val alphabets: Map[Mode, LazyList[String]] = Map(
    Mode.alpha    -> LazyList.from('a' to 'z').map(_.toString),
    Mode.alphanum -> LazyList.from('0' to 'z').map(_.toString),
    Mode.number   -> LazyList.unfold(0)(i => Some(i.toString, i + 1)),
    Mode.hex      -> LazyList.unfold(0)(i => Some((i.toHexString), i + 1)),
    Mode.octal    -> LazyList.unfold(0)(i => Some((i.toOctalString), i + 1))
  )

  def read(mode: Mode)(input: String): Option[Int] =
    alphabets(mode).zipWithIndex.find(_._1 == input).map(_._2)

  def write(mode: Mode)(input: Int): Option[String] =
    alphabets(mode).zipWithIndex.find(_._2 == input).map(_._1)
