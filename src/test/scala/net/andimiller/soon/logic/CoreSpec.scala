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
import net.andimiller.soon.models.{Grouping, Indexing, Offset}
import net.andimiller.soon.models.TimeUnit.*

import scala.concurrent.duration.*
import java.time.{ZoneOffset, ZonedDateTime}

class CoreSpec extends CatsEffectSuite:

  implicit val zones: Zones[IO] = Zones.default[IO]

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
        core               = Core.create[IO](db, Indexing.Mode.alpha, None)
        _                 <- core.run(Config.Add("event one", Offset.Single(1, Hour)))
        _                 <- core.run(Config.Add("event two", Offset.Single(1, Day)))
        _                 <- core.run(Config.Soon)
        _                 <-
          implicitly[FakeConsole].queue
            .tryTakeN(None)
            .assertEquals(
              List(
                OutStream.Std -> s"a) event one 1h${System.lineSeparator()}",
                OutStream.Std -> s"b) event two 1d${System.lineSeparator()}"
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
          core               = Core.create[IO](db, Indexing.Mode.number, None)
          _                 <- core.run(Config.Add("event one", Offset.Single(1, Hour)))
          _                 <- core.run(Config.Add("event two", Offset.Single(1, Day)))
          _                 <- core.run(Config.Soon)
          _                 <-
            implicitly[FakeConsole].queue
              .tryTakeN(None)
              .assertEquals(
                List(
                  OutStream.Std -> s"0) event one 1h${System.lineSeparator()}",
                  OutStream.Std -> s"1) event two 1d${System.lineSeparator()}"
                )
              )
          _                 <- IO.sleep(2.hours)
          _                 <- core.run(Config.Soon)
          _                 <-
            implicitly[FakeConsole].queue
              .tryTakeN(None)
              .assertEquals(
                List(
                  OutStream.Std -> s"0) event one -1h${System.lineSeparator()}",
                  OutStream.Std -> s"1) event two 1d${System.lineSeparator()}"
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
                  OutStream.Std -> s"0) event one   -3h${System.lineSeparator()}",
                  OutStream.Std -> s"1) event two   1d${System.lineSeparator()}",
                  OutStream.Std -> s"2) event three 2d${System.lineSeparator()}"
                )
              )
          _                 <- IO.sleep(5.seconds)
          _                 <- core.run(Config.Soon)
          _                 <-
            implicitly[FakeConsole].queue
              .tryTakeN(None)
              .assertEquals(
                List(
                  OutStream.Std -> s"0) event one   -3h${System.lineSeparator()}",
                  OutStream.Std -> s"1) event two   1d${System.lineSeparator()}",
                  OutStream.Std -> s"2) event three 2d${System.lineSeparator()}"
                )
              )
          _                 <- implicitly[FakeConsole].nextLine.set(Some("N"))
          _                 <- core.run(Config.Del("1"))
          _                 <-
            implicitly[FakeConsole].queue
              .tryTakeN(None)
              .assertEquals(
                List(
                  OutStream.Std -> s"Please confirm you'd like to delete this entry:${System.lineSeparator()}",
                  OutStream.Std -> s"1) event two 1d${System.lineSeparator()}",
                  OutStream.Std -> s"y/N? "
                )
              )
          _                 <- core.run(Config.Soon)
          _                 <- implicitly[FakeConsole].queue
                                 .tryTakeN(None)
                                 .assertEquals(
                                   List(
                                     OutStream.Std -> s"0) event one   -3h${System.lineSeparator()}",
                                     OutStream.Std -> s"1) event two   1d${System.lineSeparator()}",
                                     OutStream.Std -> s"2) event three 2d${System.lineSeparator()}"
                                   )
                                 )
          _                 <- implicitly[FakeConsole].nextLine.set(Some("Y"))
          _                 <- core.run(Config.Del("1"))
          _                 <-
            implicitly[FakeConsole].queue
              .tryTakeN(None)
              .assertEquals(
                List(
                  OutStream.Std -> s"Please confirm you'd like to delete this entry:${System.lineSeparator()}",
                  OutStream.Std -> s"1) event two 1d${System.lineSeparator()}",
                  OutStream.Std -> s"y/N? ",
                  OutStream.Std -> s"Event deleted${System.lineSeparator()}"
                )
              )
          _                 <- core.run(Config.Soon)
          _                 <-
            implicitly[FakeConsole].queue
              .tryTakeN(None)
              .assertEquals(
                List(
                  OutStream.Std -> s"0) event one   -3h${System.lineSeparator()}",
                  OutStream.Std -> s"1) event three 2d${System.lineSeparator()}"
                )
              )
          groupedCore        =
            Core
              .create[IO](db, Indexing.Mode.number, Some(Grouping.TrafficLight))
          _                 <- groupedCore.run(Config.Add("next week", Offset.Single(8, Day)))
          _                 <- groupedCore.run(Config.Add("week after", Offset.Single(16, Day)))
          _                 <- groupedCore.run(Config.Soon)
          _                 <-
            implicitly[FakeConsole].queue
              .tryTakeN(None)
              .assertEquals(
                List(
                  OutStream.Std -> s"Now${System.lineSeparator()}",
                  OutStream.Std -> s"0) event one -3h${System.lineSeparator()}",
                  OutStream.Std -> s"1 Week${System.lineSeparator()}",
                  OutStream.Std -> s"1) event three 2d${System.lineSeparator()}",
                  OutStream.Std -> s"2 Weeks${System.lineSeparator()}",
                  OutStream.Std -> s"2) next week  8d${System.lineSeparator()}",
                  OutStream.Std -> s"3) week after 16d${System.lineSeparator()}"
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
