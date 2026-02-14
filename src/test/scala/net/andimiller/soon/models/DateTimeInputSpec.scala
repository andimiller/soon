package net.andimiller.soon.models

import java.time.{LocalDate, LocalDateTime}

class DateTimeInputSpec extends munit.FunSuite:

  test("Parse absolute date") {
    assertEquals(
      obtained = DateTimeInput.fromString("2026-03-15"),
      expected = Right(DateTimeInput.AbsoluteDate(LocalDate.of(2026, 3, 15)))
    )
  }

  test("Parse absolute datetime with minutes") {
    assertEquals(
      obtained = DateTimeInput.fromString("2026-03-15T14:00"),
      expected = Right(
        DateTimeInput.AbsoluteDateTime(
          LocalDateTime.of(2026, 3, 15, 14, 0),
          TimeUnit.Minute
        )
      )
    )
  }

  test("Parse absolute datetime with seconds") {
    assertEquals(
      obtained = DateTimeInput.fromString("2026-03-15T14:30:45"),
      expected = Right(
        DateTimeInput.AbsoluteDateTime(
          LocalDateTime.of(2026, 3, 15, 14, 30, 45),
          TimeUnit.Second
        )
      )
    )
  }

  test("Parse relative offset") {
    assertEquals(
      obtained = DateTimeInput.fromString("3d 10h"),
      expected = Right(
        DateTimeInput.Relative(
          Offset.Add(
            Offset.Single(3, TimeUnit.Day),
            Offset.Single(10, TimeUnit.Hour)
          )
        )
      )
    )
  }

  test("Parse single relative offset") {
    assertEquals(
      obtained = DateTimeInput.fromString("5h"),
      expected = Right(DateTimeInput.Relative(Offset.Single(5, TimeUnit.Hour)))
    )
  }

  test("Fail on invalid input") {
    assert(DateTimeInput.fromString("not-a-date").isLeft)
  }

  test("Fail on incomplete date") {
    assert(DateTimeInput.fromString("2026-03").isLeft)
  }
