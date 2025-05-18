package net.andimiller.soon.logic

import cats.Applicative
import cats.effect.{Clock, IO}
import cats.implicits.*

import java.time.Instant
import scala.concurrent.duration.*

case class FakeClock(time: Instant) extends Clock[IO] {
  override def applicative: Applicative[IO] = implicitly

  override def monotonic: IO[FiniteDuration] = time.toEpochMilli.millis.pure[IO]
  override def realTime: IO[FiniteDuration]  = time.toEpochMilli.millis.pure[IO]
}
