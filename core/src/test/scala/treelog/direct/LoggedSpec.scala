package treelog.direct

class LoggedSpec extends munit.FunSuite:
  test("flatMap does not evaluate later steps after failure"):
    val output =
      Logged.failure("No two", "Could not get two").flatMap: _ =>
        fail("flatMap evaluated the next step after failure")

    assertEquals(output.result, Left("No two"))
    assertEquals(output.tree.render, "Could not get two: Failed")
