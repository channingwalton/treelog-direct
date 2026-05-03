package treelog.direct.cats

import _root_.cats.effect.IO

class LoggedTSpec extends munit.CatsEffectSuite:
  test("combines effectful steps in a for-comprehension"):
    val output =
      LoggedT.branch("Adding"):
        for
          one <- LoggedT.liftF(IO.pure(1), value => s"Got one: $value")
          two <- LoggedT.liftF(IO.pure(2), value => s"Got two: $value")
          sum <- LoggedT.success[IO, Nothing, Int](one + two, value => s"Got sum: $value")
        yield sum

    output.value.map: logged =>
      assertEquals(logged.result, Right(3))
      assertEquals(
        logged.tree.render,
        """Adding
          |  Got one: 1
          |  Got two: 2
          |  Got sum: 3""".stripMargin
      )

  test("short-circuits failures and keeps the partial tree"):
    val output =
      LoggedT.branch("Adding"):
        for
          one <- LoggedT.liftF(IO.pure(1), value => s"Got one: $value")
          _   <- LoggedT.failure[IO, String]("No two", "Could not get two")
          sum <- LoggedT.success[IO, String, Int](one + 2, value => s"Got sum: $value")
        yield sum

    output.value.map: logged =>
      assertEquals(logged.result, Left("No two"))
      assertEquals(
        logged.tree.render,
        """Adding: Failed
          |  Got one: 1
          |  Could not get two: Failed""".stripMargin
      )

  test("does not evaluate later steps after failure"):
    val output =
      LoggedT.failure[IO, String]("No two", "Could not get two").flatMap: _ =>
        fail("flatMap evaluated the next step after failure")

    output.value.map: logged =>
      assertEquals(logged.result, Left("No two"))
      assertEquals(logged.tree.render, "Could not get two: Failed")
