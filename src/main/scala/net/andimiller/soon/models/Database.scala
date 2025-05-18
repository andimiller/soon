package net.andimiller.soon.models

import cats.data.Writer
import io.circe.Codec
import cats.implicits.*
import net.andimiller.soon.models.TimeUnit

import java.time.Instant
import scala.math.Ordered.orderingToOrdered

case class Event(timestamp: Instant, granularity: TimeUnit, name: String)
    derives Codec.AsObject {
  def toOffset(now: Instant) = {
    assume(
      timestamp.isAfter(now),
      "Assuming we only convert things in the future right now"
    )
    val (offsets, _) =
      TimeUnit.values
        .filter(_ >= granularity)
        .sorted
        .toVector
        .foldLeftM(now) { case (ts, unit) =>
          val quantity      = unit.chrono.between(ts, timestamp)
          val nextTimestamp = ts.plus(quantity, unit.chrono)
          Writer
            .tell(Vector(Offset.Single(quantity.toInt, unit)))
            .as(
              nextTimestamp
            )
        }
        .run
    offsets
      .reduceOption(Offset.Add.apply)
      .getOrElse(Offset.Single(0, TimeUnit.Second))
  }

}

case class Database(
    upcoming: Vector[Event]
) derives Codec.AsObject
