package net.andimiller.soon.models

import cats.data.Writer
import io.circe.Codec
import cats.implicits.*
import net.andimiller.soon.models.TimeUnit

import java.time.Instant
import scala.math.Ordered.orderingToOrdered

case class Event(timestamp: Instant, granularity: TimeUnit, name: String)
    derives Codec.AsObject {

  private def offsetsBetween(first: Instant, second: Instant) = {
    val (offsets, _) =
      TimeUnit.values
        .filter(_ >= granularity)
        .sorted
        .toVector
        .foldLeftM(first) { case (ts, unit) =>
          val quantity      = unit.chrono.between(ts, second)
          val nextTimestamp = ts.plus(quantity, unit.chrono)
          Writer
            .tell(Vector(Offset.Single(quantity.toInt, unit)))
            .as(
              nextTimestamp
            )
        }
        .run
    offsets
  }

  def toOffset(now: Instant) = {
    val offsets = if (timestamp.isAfter(now)) {
      offsetsBetween(now, timestamp)
    } else {
      offsetsBetween(timestamp, now).map(_.invert)
    }

    offsets
      .reduceOption(Offset.Add.apply)
      .getOrElse(Offset.Single(0, TimeUnit.Second))
      .simplify
  }

}

case class Database(
    upcoming: Vector[Event]
) derives Codec.AsObject
