package net.andimiller.soon.logic

import cats.effect.Async
import cats.implicits.*
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.circe.syntax.*
import net.andimiller.soon.models.{Database, Event}

trait DB[F[_]] {
  def createIfNotExists: F[Unit]
  def getEvents: F[Vector[Event]]
  def addEvent(e: Event): F[Unit]
}

object DB {
  def create[F[_]: Async: Files](dbPath: Path): F[DB[F]] = {
    val db = new DB[F] {
      private def readDb: F[Database] =
        for
          str  <- Files[F].readUtf8(dbPath).compile.string
          json <- Async[F].fromEither(io.circe.parser.parse(str))
          db   <- Async[F].fromEither(json.as[Database])
        yield db

      private def writeDb(db: Database): F[Unit] =
        Stream
          .emit(db.asJson.spaces2)
          .covary[F]
          .through(Files[F].writeUtf8(dbPath))
          .compile
          .drain

      override def createIfNotExists: F[Unit] =
        for
          exists <- Files[F].exists(dbPath)
          _      <- if (!exists) writeDb(Database(Vector.empty)) else ().pure[F]
        yield ()

      override def getEvents: F[Vector[Event]] =
        for db <- readDb
        yield db.upcoming

      override def addEvent(e: Event): F[Unit] = {
        for
          db <- readDb
          db2 = db.copy(upcoming = db.upcoming.appended(e))
          _  <- writeDb(db2)
        yield ()
      }
    }
    db.createIfNotExists.as(db)
  }
}
