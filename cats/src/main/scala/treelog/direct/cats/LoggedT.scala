package treelog.direct.cats

import _root_.cats.{Applicative, Functor, Monad}
import _root_.cats.syntax.all.*
import treelog.direct.{Logged, Tree}

/** Transformer form of [[treelog.direct.Logged]].
  *
  * Failures represented inside `Logged` keep the partial tree. Failures raised by
  * `F` itself short-circuit before a `Logged` value exists, so no partial tree is
  * preserved.
  */
final case class LoggedT[F[_], E, A](value: F[Logged[E, A]]):
  def map[B](f: A => B)(using F: Functor[F]): LoggedT[F, E, B] =
    LoggedT(value.map(_.map(f)))

  def flatMap[EE >: E, B](f: A => LoggedT[F, EE, B])(using F: Monad[F]): LoggedT[F, EE, B] =
    LoggedT:
      value.flatMap:
        case Logged(Left(error), tree) =>
          Logged(Left(error), tree).pure[F]
        case Logged(Right(result), tree) =>
          f(result).value.map(next => Logged(next.result, Tree.combine(tree, next.tree)))

object LoggedT:
  def fromLogged[F[_]: Applicative, E, A](logged: Logged[E, A]): LoggedT[F, E, A] =
    LoggedT(logged.pure[F])

  def liftF[F[_]: Functor, A](value: F[A], label: A => String): LoggedT[F, Nothing, A] =
    LoggedT(value.map(a => Logged.success(a, label(a))))

  def success[F[_]: Applicative, E, A](value: A, label: String): LoggedT[F, E, A] =
    fromLogged(Logged.success(value, label))

  def success[F[_]: Applicative, E, A](value: A, label: A => String): LoggedT[F, E, A] =
    success(value, label(value))

  def failure[F[_]: Applicative, E](error: E, label: String): LoggedT[F, E, Nothing] =
    fromLogged(Logged.failure(error, label))

  def require[F[_]: Applicative, E](
    condition: Boolean,
    error: => E,
    successLabel: => String,
    failureLabel: => String
  ): LoggedT[F, E, Unit] =
    fromLogged(Logged.require(condition, error, successLabel, failureLabel))

  def branch[F[_]: Functor, E, A](label: String)(body: => LoggedT[F, E, A]): LoggedT[F, E, A] =
    LoggedT(body.value.map(out => Logged(out.result, Tree.branch(label, out.tree))))
