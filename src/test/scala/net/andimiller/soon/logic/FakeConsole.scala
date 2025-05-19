package net.andimiller.soon.logic

import cats.Show
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.std.{Console, Queue}
import cats.implicits.*
import net.andimiller.soon.logic.FakeConsole.OutStream

import java.nio.charset.Charset

case class FakeConsole(
    queue: Queue[IO, (OutStream, String)],
    nextLine: Ref[IO, Option[String]],
    stripColour: Boolean = true
) extends Console[IO]:

  private def str(s: String) = if (stripColour) fansi.Str(s).plainText else s

  override def println[A](a: A)(implicit S: Show[A]): IO[Unit] =
    queue.offer(OutStream.Std -> (str(a.show) + System.lineSeparator()))

  override def readLineWithCharset(charset: Charset): IO[String] =
    nextLine.get.map {
      case Some(s) => s
      case None    => ""
    }

  override def print[A](a: A)(implicit S: Show[A]): IO[Unit] =
    queue.offer(OutStream.Std -> str(a.show))

  override def error[A](a: A)(implicit S: Show[A]): IO[Unit] =
    queue.offer(OutStream.Err -> str(a.show))

  override def errorln[A](a: A)(implicit S: Show[A]): IO[Unit] =
    queue.offer(OutStream.Err -> (str(a.show) + System.lineSeparator()))

object FakeConsole:
  def create(stripColour: Boolean = true): IO[FakeConsole] =
    for
      queue <- Queue.unbounded[IO, (OutStream, String)]
      ref   <- Ref.of[IO, Option[String]](None)
    yield FakeConsole(queue, ref, stripColour)

  enum OutStream:
    case Std, Err
