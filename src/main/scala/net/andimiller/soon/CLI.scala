package net.andimiller.soon

import cats.data.ValidatedNel

import cats.implicits.*
import com.monovore.decline.*
import net.andimiller.soon.models.Offset

object CLI:

  enum Config:
    case Soon
    case Add(name: String, offset: Offset)

  given Argument[Offset] = new Argument[Offset]:
    override def read(string: String): ValidatedNel[String, Offset] =
      Offset.fromString(string).toValidatedNel

    override def defaultMetavar: String = "3d 10h"

  val cli = Command(
    name = "soon",
    header = "Simple CLI tracker for upcoming events"
  )(
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
        Opts.unit.as(
          Config.Soon
        )
      )
  )
