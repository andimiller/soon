package net.andimiller.soon
package logic

import cats.implicits.*
import cats.effect.Async
import cats.effect.kernel.Clock
import cats.effect.std.Console
import net.andimiller.soon.CLI.Config
import net.andimiller.soon.models.Event
import fansi.Color.Cyan

trait Core[F[_]]:
  def run(cmd: CLI.Config): F[Unit]

object Core:
  def create[F[_]: {Async, Console, Clock}](db: DB[F]) =
    new Core[F]:
      override def run(cmd: CLI.Config): F[Unit] =
        cmd match
          case Config.Soon              =>
            for
              d          <- db.getEvents
              now        <- Clock[F].realTimeInstant
              offsets     = d.map { e =>
                              e ->
                                e.toOffset(now)
                            }
              namePadding = offsets
                              .map { case (e, _) => e.name.size }
                              .maxOption
                              .getOrElse(0)
              _          <-
                offsets.traverse { case (e, o) =>
                  Console[F].println(
                    show"${Cyan(e.name.padTo(namePadding, ' ')).render} $o"
                  )
                }
            yield ()
          case Config.Add(name, offset) =>
            for
              now        <- Clock[F].realTimeInstant
              granularity = offset.granularity
              roundedNow  = now.truncatedTo(granularity.chrono)
              ts          = roundedNow.plusSeconds(offset.toSeconds)
              _          <- db.addEvent(Event(ts, granularity, name))
            yield ()
