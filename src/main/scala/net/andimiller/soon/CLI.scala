package net.andimiller.soon

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.*
import com.monovore.decline.*
import net.andimiller.soon.models.{DateTimeInput, Grouping, Indexing, Offset}

import scala.util.Try

object CLI:

  enum SortDimension:
    case Time
    case Alphabetic

  object SortDimension:
    given Argument[SortDimension] = new Argument[SortDimension]:
      override def read(string: String): ValidatedNel[String, SortDimension] =
        Validated
          .fromTry(
            Try {
              SortDimension.valueOf(
                string.capitalize
              )
            }
          )
          .leftMap(t => NonEmptyList.of(t.getMessage))

      override def defaultMetavar: String = "Time"

  enum Config:
    case Soon
    case Add(name: String, input: DateTimeInput)
    case Del(id: String)
    case Sort(by: SortDimension)
    case Prune

  case class SharedSettings(
      indexOverride: Option[Indexing.Mode],
      grouping: Option[Grouping]
  )

  val sharedSettings: Opts[SharedSettings] = (
    Opts
      .option[Indexing.Mode](
        "index",
        "alphabet to use when indexing",
        "i",
        "Numeric"
      )
      .orNone,
    Opts
      .option[Grouping](
        "grouping",
        "how to group events when printing",
        "g",
        "Rainbow"
      )
      .orNone
  )
    .mapN(SharedSettings)

  val cli: Command[(Config, SharedSettings)] = Command(
    name = "soon",
    header = "Simple CLI tracker for upcoming events"
  )(
    (
      Opts
        .subcommand(
          Command(name = "add", header = "add an event")(
            (
              Opts.argument[String]("name"),
              Opts.argument[DateTimeInput]("offset")
            ).mapN { case (name, input) =>
              Config.Add(name, input)
            }
          )
        )
        .orElse(
          Opts.subcommand(
            Command(name = "del", header = "delete an event")(
              Opts.argument[String]("id").map(Config.Del(_))
            )
          )
        )
        .orElse(
          Opts.subcommand(
            Command(name = "sort", header = "sort the stored events")(
              Opts.argument[SortDimension]("dimension").map(Config.Sort(_))
            )
          )
        )
        .orElse(
          Opts.subcommand(
            Command(name = "prune", header = "remove expired events")(
              Opts.unit.as(Config.Prune)
            )
          )
        )
        .orElse(
          Opts.unit.as(
            Config.Soon
          )
        ),
      sharedSettings
    ).tupled
  )
