package treelog.direct

sealed trait Tree:
  final def isSuccess: Boolean =
    Tree.isSuccess(this)

  final def render: String =
    Tree.render(this)

object Tree:
  private case object Empty extends Tree
  private final case class Leaf(label: String, success: Boolean) extends Tree
  private final case class Branch(label: String, children: Vector[Tree]) extends Tree
  private final case class Group(children: Vector[Tree]) extends Tree

  val empty: Tree =
    Empty

  def leaf(label: String, success: Boolean = true): Tree =
    Leaf(label, success)

  def branch(label: String, child: Tree): Tree =
    val children = child match
      case Empty                => Vector.empty
      case Group(grandchildren) => grandchildren
      case other                => Vector(other)

    Branch(label, children)

  def combine(left: Tree, right: Tree): Tree =
    (left, right) match
      case (Empty, r) => r
      case (l, Empty) => l
      case (Group(lc), Group(rc)) =>
        Group(lc ++ rc)
      case (Group(lc), r) =>
        Group(lc :+ r)
      case (l, Group(rc)) =>
        Group(l +: rc)
      case (l, r) =>
        Group(Vector(l, r))

  private def render(tree: Tree): String =
    lines(tree, 0).mkString("\n")

  private def isSuccess(tree: Tree): Boolean =
    tree match
      case Empty                    => true
      case Leaf(_, success)         => success
      case Branch(_, children)      => children.forall(isSuccess)
      case Group(children)          => children.forall(isSuccess)

  private def lines(tree: Tree, depth: Int): Vector[String] =
    tree match
      case Empty =>
        Vector.empty
      case Leaf(label, success) =>
        Vector(s"${indent(depth)}${show(label, success)}")
      case Branch(label, children) =>
        val ownLine = Vector(s"${indent(depth)}${show(label, children.forall(isSuccess))}")
        ownLine ++ children.flatMap(child => lines(child, depth + 1))
      case Group(children) =>
        children.flatMap(child => lines(child, depth))

  private def show(label: String, success: Boolean): String =
    if success then label else s"$label: Failed"

  private def indent(depth: Int): String =
    "  " * depth
