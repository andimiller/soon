package net.andimiller.soon.models

import cats.implicits.*
import java.time.Instant
import java.time.temporal.ChronoUnit

class EventSpec extends munit.FunSuite {

  test("Should round to granularity") {
    val now = Instant.ofEpochSecond(1748728693L)

    val tests = Map(
      Event(
        now
          .plus(2, ChronoUnit.DAYS)
          .plus(5, ChronoUnit.HOURS)
          .plus(7, ChronoUnit.MINUTES),
        TimeUnit.Day,
        "example"
      ) -> "3d",
      Event(
        now
          .plus(2, ChronoUnit.DAYS),
        TimeUnit.Day,
        "example"
      ) -> "2d",
      Event(
        now
          .plus(1, ChronoUnit.HOURS),
        TimeUnit.Day,
        "example"
      ) -> "1d"
    )

    tests.foreach { case (event, expected) =>
      assertEquals(
        obtained =
          event.toOffset(now).roundUpGranularity(event.granularity).show,
        expected = expected
      )
    }

  }

}
