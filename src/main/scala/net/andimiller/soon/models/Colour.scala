package net.andimiller.soon.models

import fansi.{Attr, Color}
import io.circe.{Encoder, Decoder}
import scala.util.Try

enum Colour(attr: Attr):
  case Black        extends Colour(Color.Black)
  case Red          extends Colour(Color.Red)
  case Green        extends Colour(Color.Green)
  case Yellow       extends Colour(Color.Yellow)
  case Blue         extends Colour(Color.Blue)
  case Magenta      extends Colour(Color.Magenta)
  case Cyan         extends Colour(Color.Cyan)
  case LightGray    extends Colour(Color.LightGray)
  case DarkGray     extends Colour(Color.DarkGray)
  case LightRed     extends Colour(Color.LightRed)
  case LightGreen   extends Colour(Color.LightGreen)
  case LightYellow  extends Colour(Color.LightYellow)
  case LightBlue    extends Colour(Color.LightBlue)
  case LightMagenta extends Colour(Color.LightMagenta)
  case LightCyan    extends Colour(Color.LightCyan)
  case White        extends Colour(Color.White)

  def apply(s: String): String = attr.apply(s).render

object Colour:
  given Encoder[Colour] = Encoder.encodeString.contramap(_.toString)
  given Decoder[Colour] = Decoder.decodeString.emapTry { s =>
    Try { Colour.valueOf(s) }
  }
