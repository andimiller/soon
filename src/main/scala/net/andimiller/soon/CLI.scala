package net.andimiller.soon

import cats.data.ValidatedNel
import cats.implicits.*
import com.monovore.decline.*
import net.andimiller.soon.models.{Indexing, Offset}

object CLI:

  enum Config:
    case Soon
    case Add(name: String, offset: Offset)
    case Del(id: String)

  given Argument[Offset] = new Argument[Offset]:
    override def read(string: String): ValidatedNel[String, Offset] =
      Offset.fromString(string).toValidatedNel

    override def defaultMetavar: String = "3d 10h"

  case class SharedSettings(
      indexOverride: Option[Indexing.Mode]
  )

  val sharedSettings: Opts[SharedSettings] = (
    Opts
      .option[Indexing.Mode](
        "index",
        "alphabet to use when indexing",
        "i",
        "Numeric"
      )
      .orNone
    )
    .map(SharedSettings(_))

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
              Opts.argument[Offset]("offset")
            ).mapN { case (name, offset) =>
              Config.Add(name, offset)
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
          Opts.unit.as(
            Config.Soon
          )
        ),
      sharedSettings
    ).tupled
  )
