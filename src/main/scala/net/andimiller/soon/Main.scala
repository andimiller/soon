package net.andimiller.soon

import cats.effect.std.Env
import cats.effect.Async
import cats.implicits.*
import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path
import net.andimiller.soon.logic.{Core, DB}
import net.andimiller.soon.models.Indexing

object Main extends IOApp:

  def configPath[F[_]: {Async, Env}] =
    for
      env <- Env[F].get("SOON_CONFIG")
      p   <- env.fold(Async[F].delay {
               Path(System.getProperty("user.home")) / ".soon"
             })(
               Path(_).pure[F]
             )
    yield p

  override def run(args: List[String]): IO[ExitCode] =
    CLI.cli.parse(args, sys.env) match
      case Left(value)            => IO.println(value).as(ExitCode.Error)
      case Right((cmd, settings)) =>
        for
          c   <- configPath[IO]
          db  <- DB.create[IO](c)
          core = Core.create[IO](
                   db,
                   settings.indexOverride.getOrElse(Indexing.Mode.alpha)
                 )
          _   <- core.run(cmd)
        yield ExitCode.Success
