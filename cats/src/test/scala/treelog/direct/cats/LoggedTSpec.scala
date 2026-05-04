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
      LoggedT.failure[IO, String]("No two", "Could not get two").flatMap: _ =>
        fail("flatMap evaluated the next step after failure")

    output.value.map: logged =>
      assertEquals(logged.result, Left("No two"))
      assertEquals(logged.tree.render, "Could not get two: Failed")

  test("maps the outer effect"):
    val output =
      LoggedT.success[IO, String, Int](1, "Got one").mapK(
        new FunctionK[IO, Future]:
          def apply[A](fa: IO[A]): Future[A] =
            fa.unsafeToFuture()
      )

    IO.fromFuture(IO(output.value)).map: logged =>
      assertEquals(logged.result, Right(1))
      assertEquals(logged.tree.render, "Got one")
