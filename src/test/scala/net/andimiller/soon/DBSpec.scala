package net.andimiller.soon

import cats.effect.IO
import fs2.io.file.Files
import munit.CatsEffectSuite
import net.andimiller.soon.logic.DB
import net.andimiller.soon.models.Event
import net.andimiller.soon.models.TimeUnit.Second

import java.time.Instant

class DBSpec extends CatsEffectSuite:

  test(
    "Should be able to initialise an empty db, store stuff and read it back out"
  ) {
    Files[IO].tempFile.use { dbPath =>
      println(dbPath)
      for
        _     <- Files[IO].delete(dbPath)
        now   <- IO { Instant.now() }
        db    <- DB.create[IO](dbPath)
        _     <- db.getEvents.assertEquals(Vector.empty)
        event1 = Event(now, Second, "example 1")
        _     <- db.addEvent(event1)
        _     <- db.getEvents.assertEquals(Vector(event1))
        event2 = Event(now, Second, "example 2")
        _     <- db.addEvent(event2)
        _     <- db.getEvents.assertEquals(Vector(event1, event2))
      yield ()
    }
  }
