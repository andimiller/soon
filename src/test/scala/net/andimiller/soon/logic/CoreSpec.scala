package net.andimiller.soon.logic

import cats.effect.*
import cats.effect.kernel.testkit.TestContext
import cats.effect.std.Console
import cats.effect.testkit.TestControl
import fs2.io.file.Files
import cats.effect.Temporal
import munit.CatsEffectSuite
import net.andimiller.soon.CLI.Config
import net.andimiller.soon.logic.FakeConsole.OutStream
import net.andimiller.soon.models.Offset
import net.andimiller.soon.models.TimeUnit.*
import scala.concurrent.duration.*

import java.time.{ZoneOffset, ZonedDateTime}

class CoreSpec extends CatsEffectSuite:

  test("Add events and print them out") {

    Files[IO].tempFile.use { dbPath =>
      for {
        _                 <- Files[IO].delete(dbPath)
        given FakeConsole <- FakeConsole.create()
        given FakeClock    =
          FakeClock(
            ZonedDateTime.of(2025, 5, 18, 20, 0, 0, 0, ZoneOffset.UTC).toInstant
          )
        db                <- DB.create[IO](dbPath)
        core               = Core.create[IO](db)
        _                 <- core.run(Config.Add("event one", Offset.Single(1, Hour)))
        _                 <- core.run(Config.Add("event two", Offset.Single(1, Day)))
        _                 <- core.run(Config.Soon)
        _                 <-
          implicitly[FakeConsole].queue
            .tryTakeN(None)
            .assertEquals(
              List(
                OutStream.Std -> s"event one 1h${System.lineSeparator()}",
                OutStream.Std -> s"event two 4h${System.lineSeparator()}"
              )
            )
      } yield ()
    }

  }

  test("Add events and do some time travel") {

    val run =
      Files[IO].tempFile.use { dbPath =>
        for {
          _                 <- Files[IO].delete(dbPath)
          given FakeConsole <- FakeConsole.create()
          db                <- DB.create[IO](dbPath)
          core               = Core.create[IO](db)
          _                 <- core.run(Config.Add("event one", Offset.Single(1, Hour)))
          _                 <- core.run(Config.Add("event two", Offset.Single(1, Day)))
          _                 <- core.run(Config.Soon)
          _                 <-
            implicitly[FakeConsole].queue
              .tryTakeN(None)
              .assertEquals(
                List(
                  OutStream.Std -> s"event one 1h${System.lineSeparator()}",
                  OutStream.Std -> s"event two 1d${System.lineSeparator()}"
                )
              )
          _                 <- IO.sleep(2.hours)
          _                 <- core.run(Config.Soon)
          _                 <-
            implicitly[FakeConsole].queue
              .tryTakeN(None)
              .assertEquals(
                List(
                  OutStream.Std -> s"event one -1h${System.lineSeparator()}",
                  OutStream.Std -> s"event two 22h${System.lineSeparator()}"
                )
              )
          _                 <- IO.sleep(2.hours)
          _                 <- core.run(Config.Add("event three", Offset.Single(2, Day)))
          _                 <- core.run(Config.Soon)
          _                 <-
            implicitly[FakeConsole].queue
              .tryTakeN(None)
              .assertEquals(
                List(
                  OutStream.Std -> s"event one   -3h${System.lineSeparator()}",
                  OutStream.Std -> s"event two   20h${System.lineSeparator()}",
                  OutStream.Std -> s"event three 1d 20h${System.lineSeparator()}"
                )
              )
          _                 <- IO.sleep(5.seconds)
          _                 <- core.run(Config.Soon)
          _                 <-
            implicitly[FakeConsole].queue
              .tryTakeN(None)
              .assertEquals(
                List(
                  OutStream.Std -> s"event one   -3h -5s${System.lineSeparator()}",
                  OutStream.Std -> s"event two   19h 59m 55s${System.lineSeparator()}",
                  OutStream.Std -> s"event three 1d 19h 59m 55s${System.lineSeparator()}"
                )
              )
        } yield ()
      }

    for
      control <- TestControl.execute(run)
      _       <- control.tickAll
      _       <- control.results
                   .map(_.get)
                   .assertEquals(Outcome.Succeeded[cats.Id, Throwable, Unit](()))
    yield ()

  }
