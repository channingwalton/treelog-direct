package treelog.direct

class LoggedSpec extends munit.FunSuite:
  test("constructs pure values without log lines"):
    val output = Logged.pure[String, Int](1)

    assertEquals(output.result, Right(1))
    assertEquals(output.tree.render, "")

  test("constructs logged values from either"):
    val success = Logged.fromEither[String, Int](Right(1), "Got one")
    val failure = Logged.fromEither[String, Int](Left("No one"), "Could not get one")

    assertEquals(success.result, Right(1))
    assertEquals(success.tree.render, "Got one")
    assertEquals(failure.result, Left("No one"))
    assertEquals(failure.tree.render, "Could not get one: Failed")

  test("maps failures"):
    val output =
      Logged.failure("No two", "Could not get two").leftMap(_.length)

    assertEquals(output.result, Left(6))
    assertEquals(output.tree.render, "Could not get two: Failed")

  test("skips empty branch bodies"):
    val output =
      Logged.branch("Empty")(Logged.pure[String, Int](1))

    assertEquals(output.result, Right(1))
    assertEquals(output.tree.render, "")

  test("flatMap does not evaluate later steps after failure"):
    val output =
      Logged.failure("No two", "Could not get two").flatMap: _ =>
        fail("flatMap evaluated the next step after failure")

    assertEquals(output.result, Left("No two"))
    assertEquals(output.tree.render, "Could not get two: Failed")
