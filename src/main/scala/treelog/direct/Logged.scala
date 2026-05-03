package treelog.direct

final case class Logged[+E, +A](result: Either[E, A], tree: Tree):
  def map[B](f: A => B): Logged[E, B] =
    result match
      case Right(value) => Logged(Right(f(value)), tree)
      case Left(error)  => Logged(Left(error), tree)

  def flatMap[EE >: E, B](f: A => Logged[EE, B]): Logged[EE, B] =
    result match
      case Left(error) =>
        Logged(Left(error), tree)
      case Right(value) =>
        val next = f(value)
        Logged(next.result, Tree.combine(tree, next.tree))

object Logged:
  def success[A](value: A, label: String): Logged[Nothing, A] =
    Logged(Right(value), Tree.leaf(label, success = true))

  def success[A](value: A, label: A => String): Logged[Nothing, A] =
    success(value, label(value))

  def failure[E](error: E, label: String): Logged[E, Nothing] =
    Logged(Left(error), Tree.leaf(label, success = false))

  def branch[E, A](label: String)(body: => Logged[E, A]): Logged[E, A] =
    val out = body
    Logged(out.result, Tree.branch(label, out.tree))

  def require[E](
    condition: Boolean,
    error: => E,
    successLabel: => String,
    failureLabel: => String
  ): Logged[E, Unit] =
    if condition then success((), successLabel)
    else failure(error, failureLabel)

export Logged.{branch, failure, require, success}
