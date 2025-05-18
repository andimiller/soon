package net.andimiller.soon.models

import org.scalacheck.Prop.forAll
import cats.implicits.*
import org.scalacheck.{Arbitrary, Gen}

class TimeUnitSpec extends munit.ScalaCheckSuite:

  given Arbitrary[TimeUnit] = Arbitrary(Gen.oneOf(TimeUnit.values))

  property("Should parse and serialize all types") {
    forAll { (t: TimeUnit) =>
      TimeUnit.fromString(t.show) == Right(t)
    }
  }
