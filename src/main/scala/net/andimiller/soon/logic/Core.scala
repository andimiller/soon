package net.andimiller.soon
package logic

import cats.implicits.*
import cats.effect.Async
import cats.effect.kernel.Clock
import cats.effect.std.Console
import net.andimiller.soon.CLI.{Config, SortDimension}
import net.andimiller.soon.models.{Event, Grouping, Indexing}
import fansi.Color.Cyan

trait Core[F[_]]:
  def run(cmd: CLI.Config): F[Unit]

object Core:
  def addIndex(mode: Indexing.Mode)(
      events: Vector[Event]
  ): Vector[(String, Event)] =
    (Indexing.alphabets(mode) ++ LazyList.continually("?")).zip(events).toVector

  def create[F[_]: {Async, Console, Clock}](
      db: DB[F],
      mode: Indexing.Mode,
      grouping: Option[Grouping]
  ) =
    new Core[F]:
      override def run(cmd: CLI.Config): F[Unit] =
        cmd match
          case Config.Soon if grouping.isDefined =>
            for
              d      <- db.getEvents
              now    <- Clock[F].realTimeInstant
              indexes = addIndex(mode)(d).map { case (v, k) => k -> v }.toMap

              grouped = grouping.get.buckets.group(d)(now)
              _      <-
                grouped.toVector.traverse { case (bucket, events) =>
                  val namePadding =
                    events.map(_.name.length).maxOption.getOrElse(0)
                  Console[F].println(bucket.colour(bucket.name)) *>
                    events.traverse { event =>
                      val i = indexes(event)
                      Console[F].println(
                        show"$i) ${bucket.colour(event.name.padTo(namePadding, ' '))} ${event.toOffset(now).roundUpGranularity(event.granularity)}"
                      )
                    }
                }
            yield ()
          case Config.Soon                       =>
            for
              d          <- db.getEvents
              now        <- Clock[F].realTimeInstant
              offsets     = addIndex(mode)(d).map { (i, e) =>
                              i -> (e -> e.toOffset(now))
                            }
              namePadding = offsets
                              .map { case (_, (e, _)) => e.name.length }
                              .maxOption
                              .getOrElse(0)
              _          <-
                offsets.traverse { case (i, (e, o)) =>
                  Console[F].println(
                    show"$i) ${Cyan(e.name.padTo(namePadding, ' ')).render} ${o.roundUpGranularity(e.granularity)}"
                  )
                }
            yield ()
          case Config.Add(name, offset)          =>
            for
              now        <- Clock[F].realTimeInstant
              granularity = offset.granularity
              roundedNow  = now.truncatedTo(granularity.chrono)
              ts          = roundedNow.plusSeconds(offset.toSeconds)
              _          <- db.addEvent(Event(ts, granularity, name))
            yield ()
          case Config.Sort(by)                   =>
            for {
              d     <- db.getEvents
              sorted = by match
                         case SortDimension.Time       => d.sortBy(_.timestamp)
                         case SortDimension.Alphabetic => d.sortBy(_.name)
              _     <- db.setEvents(sorted)
              _     <- run(Config.Soon)
            } yield ()
          case Config.Del(idxStr)                =>
            for
              d      <- db.getEvents
              now    <- Clock[F].realTimeInstant
              offsets = addIndex(mode)(d).map { (i, e) =>
                          i -> (e -> e.toOffset(now))
                        }
              target  = offsets.collectFirst {
                          case (i, (e, o)) if i == idxStr =>
                            (i, (e, o))
                        }
              _      <- target match {
                          case Some(t) =>
                            val (i, (e, o)) = t
                            val idx         = Indexing.read(mode)(i).get
                            for {
                              _    <-
                                Console[F].println(
                                  "Please confirm you'd like to delete this entry:"
                                )
                              _    <-
                                Console[F].println(
                                  show"$i) ${Cyan(e.name).render} ${o.roundUpGranularity(e.granularity)}"
                                )
                              _    <- Console[F].print("y/N? ")
                              bool <- Console[F].readLine.map {
                                        case s
                                            if s.startsWith("y") || s.startsWith(
                                              "Y"
                                            ) =>
                                          true
                                        case _ => false
                                      }
                              _    <- if (bool)
                                        db.deleteEvent(idx) *> Console[F].println(
                                          "Event deleted"
                                        )
                                      else ().pure[F]
                            } yield ()
                          case None    =>
                            Console[F].errorln(
                              s"Could not find corresponding event for index $idxStr"
                            )
                        }
            yield ()
