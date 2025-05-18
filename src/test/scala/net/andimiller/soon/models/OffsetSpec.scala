package net.andimiller.soon.models

import cats.implicits.*
import net.andimiller.soon.models.TimeUnit.{Day, Hour, Minute}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

class OffsetSpec extends munit.ScalaCheckSuite:

  test("Should be able to simplify an Offset") {
    assertEquals(
      obtained = Offset
        .Add(
          Offset.Single(10, Hour),
          Offset.Single(5, Hour)
        )
        .simplify,
      expected = Offset.Single(15, Hour)
    )
  }

  test("Should be able to simplify an Offset which spans units") {
    assertEquals(
      obtained = Offset
        .Add(
          Offset.Single(20, Hour),
          Offset.Single(20, Hour)
        )
        .simplify,
      expected = Offset.Add(
        Offset.Single(1, Day),
        Offset.Single(16, Hour)
      )
    )
  }

  test("Iterator should iterate all singles") {
    assertEquals(
      obtained = Offset
        .Add(
          Offset.Single(1, Day),
          Offset.Add(Offset.Single(2, Hour), Offset.Single(3, Minute))
        )
        .iterator,
      expected = LazyList(
        Offset.Single(1, Day),
        Offset.Single(2, Hour),
        Offset.Single(3, Minute)
      )
    )
  }

  given timeUnit: Arbitrary[TimeUnit] = Arbitrary(Gen.oneOf(TimeUnit.values))
  given Arbitrary[Offset]             = Arbitrary(
    Gen
      .recursive[Offset] { recurse =>
        val single = for
          u     <- timeUnit.arbitrary
          value <- Gen.choose(1, u.maxValue)
        yield Offset.Single(value, u)

        val join = for
          a <- recurse
          b <- recurse
        yield Offset.Add(a, b)

        Gen.oneOf(single, join)
      }
      .map(_.simplify)
  )

  property("Should round trip show and parse") {
    forAll { (o: Offset) =>
      Offset.fromString(o.show).map(_.simplify) == Right(o)
    }
  }
