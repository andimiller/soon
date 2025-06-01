package net.andimiller.soon.logic

import cats.Monad
import cats.implicits.*
import cats.effect.Async
import fs2.io.file.{Files, Path}

import java.time.ZoneId

trait Zones[F[_]] {
  def defaultTimezone: F[ZoneId]
}

object Zones {
  def apply[F[_]](implicit zones: Zones[F]): Zones[F] = zones

  def localtime[F[_]: {Files, Monad}] =
    for
      link      <- Files[F].realPath(Path("/etc/localtime"))
      stringForm = link.toString
      stripped   = stringForm.replaceAll(".*zoneinfo/", "")
      zone       = ZoneId.of(stripped)
    yield zone

  def linux[F[_]: {Files, Async}] = new Zones[F] {
    override def defaultTimezone: F[ZoneId] =
      localtime[F].attemptT.recover { e =>
        ZoneId.systemDefault()
      }.rethrowT
  }

  def default[F[_]: {Async}] = new Zones[F] {
    override def defaultTimezone: F[ZoneId] = ZoneId.systemDefault().pure[F]
  }
}
