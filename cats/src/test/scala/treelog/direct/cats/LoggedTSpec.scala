package treelog.direct.cats

import _root_.cats.Monad
import _root_.cats.arrow.FunctionK
import _root_.cats.effect.IO
import scala.concurrent.Future

class LoggedTSpec extends munit.CatsEffectSuite:
  test("provides cats functor and monad instances"):
    summon[Monad[[A] =>> LoggedT[IO, String, A]]]

    val output =
      for
        one <- LoggedT.success[IO, String, Int](1, "Got one")
        two <- LoggedT.success[IO, String, Int](2, "Got two")
      yield one + two

    output.value.map: logged =>
      assertEquals(logged.result, Right(3))
      assertEquals(logged.tree.render, "Got one\nGot two")

  test("infers lifted effect error type from expected result type"):
    val output: LoggedT[IO, String, Int] =
      LoggedT.liftF(IO.pure(1), value => s"Got one: $value")

    output.value.map: logged =>
      assertEquals(logged.result, Right(1))
      assertEquals(logged.tree.render, "Got one: 1")

  test("lifts unlogged effects"):
    val output: LoggedT[IO, String, Int] =
      LoggedT.liftF_(IO.pure(1))

    output.value.map: logged =>
      assertEquals(logged.result, Right(1))
      assertEquals(logged.tree.render, "")

  test("constructs logged values from either"):
    val success =
      LoggedT.fromEither[IO, String, Int](Right(1), value => s"Got one: $value", error => s"Could not get one: $error")
    val failure =
      LoggedT.fromEither[IO, String, Int](Left("No one"), value => s"Got one: $value", error => s"Could not get one: $error")

    success.value.flatMap: successLogged =>
      failure.value.map: failureLogged =>
        assertEquals(successLogged.result, Right(1))
        assertEquals(successLogged.tree.render, "Got one: 1")
        assertEquals(failureLogged.result, Left("No one"))
        assertEquals(failureLogged.tree.render, "Could not get one: No one: Failed")

  test("constructs logged values from effectful either"):
    val success =
      LoggedT.fromEitherF[IO, String, Int](IO.pure(Right(1)), value => s"Got one: $value", error => s"Could not get one: $error")
    val failure =
      LoggedT.fromEitherF[IO, String, Int](IO.pure(Left("No one")), value => s"Got one: $value", error => s"Could not get one: $error")

    success.value.flatMap: successLogged =>
      failure.value.map: failureLogged =>
        assertEquals(successLogged.result, Right(1))
        assertEquals(successLogged.tree.render, "Got one: 1")
        assertEquals(failureLogged.result, Left("No one"))
        assertEquals(failureLogged.tree.render, "Could not get one: No one: Failed")

  test("maps logged failure errors"):
    val output =
      LoggedT
        .failure[IO, String]("No two", "Could not get two")
        .leftMap(_.length)

    output.value.map: logged =>
      assertEquals(logged.result, Left(6))
      assertEquals(logged.tree.render, "Could not get two: Failed")

  test("captures attempted effects as logged values"):
    val success =
      LoggedT.attemptF[IO, String, Int](
        IO.pure(1),
        throwable => s"Effect failed: ${throwable.getMessage}",
        value => s"Got value: $value",
        error => error
      )
    val failure =
      LoggedT.attemptF[IO, String, Int](
        IO.raiseError(new RuntimeException("boom")),
        throwable => s"Effect failed: ${throwable.getMessage}",
        value => s"Got value: $value",
        error => error
      )

    success.value.flatMap: successLogged =>
      failure.value.map: failureLogged =>
        assertEquals(successLogged.result, Right(1))
        assertEquals(successLogged.tree.render, "Got value: 1")
        assertEquals(failureLogged.result, Left("Effect failed: boom"))
        assertEquals(failureLogged.tree.render, "Effect failed: boom: Failed")

  test("combines effectful steps in a for-comprehension"):
    val output =
      LoggedT.branch("Adding"):
        for
          one <- LoggedT.liftF[IO, String, Int](IO.pure(1), value => s"Got one: $value")
          two <- LoggedT.liftF[IO, String, Int](IO.pure(2), value => s"Got two: $value")
          sum <- LoggedT.success[IO, String, Int](one + two, value => s"Got sum: $value")
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
          one <- LoggedT.liftF[IO, String, Int](IO.pure(1), value => s"Got one: $value")
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
      LoggedT
        .failure[IO, String]("No two", "Could not get two")
        .flatMap: _ =>
          fail("flatMap evaluated the next step after failure")

    output.value.map: logged =>
      assertEquals(logged.result, Left("No two"))
      assertEquals(logged.tree.render, "Could not get two: Failed")

  test("maps the outer effect"):
    val output =
      LoggedT
        .success[IO, String, Int](1, "Got one")
        .mapK(
          new FunctionK[IO, Future]:
            def apply[A](fa: IO[A]): Future[A] =
              fa.unsafeToFuture()
        )

    IO.fromFuture(IO(output.value))
      .map: logged =>
        assertEquals(logged.result, Right(1))
        assertEquals(logged.tree.render, "Got one")
