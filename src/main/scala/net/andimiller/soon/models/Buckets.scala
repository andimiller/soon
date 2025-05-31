package net.andimiller.soon.models

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import io.circe.{Decoder, Encoder}
import com.monovore.decline.Argument

import scala.collection.immutable.ListMap
import scala.util.Try
import java.time.Instant
import scala.collection.SortedMap
import scala.math.Ordering.Implicits.*

case class Bucket(name: String, colour: Colour, offset: Offset)
    derives Encoder.AsObject,
      Decoder
object Bucket:
  given Ordering[Bucket] = Ordering.by(_.offset)

case class Buckets(buckets: List[Bucket]) derives Encoder.AsObject, Decoder:
  lazy val sortedBuckets: List[Bucket] = buckets.sortBy(_.offset)
  def group(
      events: Vector[Event]
  )(now: Instant): SortedMap[Bucket, Vector[Event]] = {
    SortedMap.from(
      events.groupBy { e =>
        sortedBuckets
          .find { bucket =>
            e.toOffset(now) <= bucket.offset
          }
          .getOrElse(sortedBuckets.last)
      }
    )
  }

enum Grouping(val buckets: Buckets):
  case TrafficLight
      extends Grouping(
        Buckets(
          List(
            Bucket("Now", Colour.Red, Offset.Single(0, TimeUnit.Day)),
            Bucket("1 Week", Colour.Yellow, Offset.Single(7, TimeUnit.Day)),
            Bucket("2 Weeks", Colour.Green, Offset.Single(14, TimeUnit.Day))
          )
        )
      )
  case Rainbow
      extends Grouping(
        Buckets(
          List(
            Bucket("Week 1", Colour.Red, Offset.Single(7 * 1, TimeUnit.Day)),
            Bucket(
              "Week 2",
              Colour.LightRed,
              Offset.Single(7 * 2, TimeUnit.Day)
            ),
            Bucket("Week 3", Colour.Yellow, Offset.Single(7 * 3, TimeUnit.Day)),
            Bucket("Week 4", Colour.Green, Offset.Single(7 * 4, TimeUnit.Day)),
            Bucket("Week 5", Colour.Blue, Offset.Single(7 * 5, TimeUnit.Day)),
            Bucket(
              "Week 6",
              Colour.Magenta,
              Offset.Single(7 * 6, TimeUnit.Day)
            ),
            Bucket(
              "Week 7",
              Colour.LightMagenta,
              Offset.Single(7 * 7, TimeUnit.Day)
            )
          )
        )
      )

object Grouping:
  given Argument[Grouping] = new Argument[Grouping]:
    override def read(string: String): ValidatedNel[String, Grouping] =
      Validated
        .fromTry(
          Try {
            Grouping.valueOf(string)
          }
        )
        .leftMap(t => NonEmptyList.of(t.getMessage))

    override def defaultMetavar: String = "Rainbow"
