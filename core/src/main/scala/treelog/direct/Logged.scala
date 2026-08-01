package treelog.direct

final case class Logged[+E, +A](result: Either[E, A], tree: Tree):
  scala.Predef.require(
    result.isRight == tree.isSuccess,
    "Logged result and tree success must agree"
  )

  def map[B](f: A => B): Logged[E, B] =
    result match
      case Right(value) => Logged(Right(f(value)), tree)
      case Left(error)  => Logged(Left(error), tree)

  def leftMap[EE](f: E => EE): Logged[EE, A] =
    result match
      case Right(value) => Logged(Right(value), tree)
      case Left(error)  => Logged(Left(f(error)), tree)

  def flatMap[EE >: E, B](f: A => Logged[EE, B]): Logged[EE, B] =
    result match
      case Left(error)  =>
        Logged(Left(error), tree)
      case Right(value) =>
        val next = f(value)
        Logged(next.result, Tree.combine(tree, next.tree))

object Logged:
  def pure[E, A](value: A): Logged[E, A] =
    Logged(Right(value), Tree.empty)

  def fromEither[E, A](value: Either[E, A], label: String): Logged[E, A] =
    value match
      case Right(successValue) => success(successValue, label)
      case Left(error)         => failure(error, label)

  def success[A](value: A, label: String): Logged[Nothing, A] =
    Logged(Right(value), Tree.leaf(label, success = true))

  def success[A](value: A, label: A => String): Logged[Nothing, A] =
    success(value, label(value))

  def failure[E](error: E, label: String): Logged[E, Nothing] =
    Logged(Left(error), Tree.leaf(label, success = false))

  /** Wraps the body log under a branch.
    *
    * Exceptions thrown while evaluating `body` are not captured. Use typed failures when the partial tree must be preserved.
    */
  def branch[E, A](label: String)(body: => Logged[E, A]): Logged[E, A] =
    val out = body
    Logged(out.result, Tree.branch(label, out.tree))

  /** Creates a logged requirement check.
    *
    * This is intentionally available as `Logged.require` but not exported by `treelog.direct.*`, so wildcard imports do not shadow
    * `scala.Predef.require`.
    */
  def require[E](
    condition: Boolean,
    error: => E,
    successLabel: => String,
    failureLabel: => String
  ): Logged[E, Unit] =
    if condition then success((), successLabel)
    else failure(error, failureLabel)

export Logged.{branch, failure, success}
