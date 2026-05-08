package treelog.direct.cats

import _root_.cats.{Applicative, Functor, Monad, MonadThrow}
import _root_.cats.syntax.all.*
import _root_.cats.arrow.FunctionK
import treelog.direct.{Logged, Tree}

/** Transformer form of [[treelog.direct.Logged]].
  *
  * Failures represented inside `Logged` keep the partial tree. Failures raised by `F` itself short-circuit before a `Logged` value exists,
  * so no partial tree is preserved.
  */
final case class LoggedT[F[_], E, A](value: F[Logged[E, A]]):
  def map[B](f: A => B)(using F: Functor[F]): LoggedT[F, E, B] =
    LoggedT(value.map(_.map(f)))

  def leftMap[EE](f: E => EE)(using F: Functor[F]): LoggedT[F, EE, A] =
    LoggedT(value.map(_.leftMap(f)))

  def flatMap[EE >: E, B](f: A => LoggedT[F, EE, B])(using F: Monad[F]): LoggedT[F, EE, B] =
    LoggedT:
      value.flatMap:
        case Logged(Left(error), tree)   =>
          Logged(Left(error), tree).pure[F]
        case Logged(Right(result), tree) =>
          f(result).value.map(next => Logged(next.result, Tree.combine(tree, next.tree)))

  def mapK[G[_]](f: FunctionK[F, G]): LoggedT[G, E, A] =
    LoggedT(f(value))

object LoggedT:
  given loggedTFunctor[F[_]: Functor, E]: Functor[[A] =>> LoggedT[F, E, A]] with
    def map[A, B](fa: LoggedT[F, E, A])(f: A => B): LoggedT[F, E, B] =
      fa.map(f)

  given loggedTMonad[F[_]: Monad, E]: Monad[[A] =>> LoggedT[F, E, A]] with
    def pure[A](value: A): LoggedT[F, E, A] =
      LoggedT.pure(value)

    def flatMap[A, B](fa: LoggedT[F, E, A])(f: A => LoggedT[F, E, B]): LoggedT[F, E, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => LoggedT[F, E, Either[A, B]]): LoggedT[F, E, B] =
      LoggedT:
        Monad[F].tailRecM((a, Tree.empty)) { case (current, tree) =>
          f(current).value.map {
            case Logged(Left(error), nextTree)         =>
              Right(Logged(Left(error), Tree.combine(tree, nextTree)))
            case Logged(Right(Left(next)), nextTree)   =>
              Left((next, Tree.combine(tree, nextTree)))
            case Logged(Right(Right(value)), nextTree) =>
              Right(Logged(Right(value), Tree.combine(tree, nextTree)))
          }
        }

  def pure[F[_]: Applicative, E, A](value: A): LoggedT[F, E, A] =
    fromLogged(Logged.pure(value))

  def fromLogged[F[_]: Applicative, E, A](logged: Logged[E, A]): LoggedT[F, E, A] =
    LoggedT(logged.pure[F])

  def fromEither[F[_]: Applicative, E, A](
    value: Either[E, A],
    successLabel: A => String,
    failureLabel: E => String
  ): LoggedT[F, E, A] =
    fromLogged:
      value match
        case Right(successValue) => Logged.success(successValue, successLabel(successValue))
        case Left(error)         => Logged.failure(error, failureLabel(error))

  def fromEitherF[F[_]: Functor, E, A](
    value: F[Either[E, A]],
    successLabel: A => String,
    failureLabel: E => String
  ): LoggedT[F, E, A] =
    LoggedT:
      value.map:
        case Right(successValue) => Logged.success(successValue, successLabel(successValue))
        case Left(error)         => Logged.failure(error, failureLabel(error))

  def liftF[F[_]: Functor, E, A](value: F[A], label: A => String): LoggedT[F, E, A] =
    LoggedT(value.map(a => Logged.success(a, label(a))))

  def liftF_[F[_]: Functor, E, A](value: F[A]): LoggedT[F, E, A] =
    LoggedT(value.map(Logged.pure(_)))

  def attemptF[F[_]: MonadThrow, E, A](
    value: F[A],
    onThrowable: Throwable => E,
    successLabel: A => String,
    failureLabel: E => String
  ): LoggedT[F, E, A] =
    LoggedT:
      value.attempt.map:
        case Right(successValue) =>
          Logged.success(successValue, successLabel(successValue))
        case Left(throwable)     =>
          val error = onThrowable(throwable)
          Logged.failure(error, failureLabel(error))

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
