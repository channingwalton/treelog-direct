package treelog.direct

import scala.annotation.tailrec

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
    child match
      case Empty                => Empty
      case Group(grandchildren) => Branch(label, grandchildren)
      case other                => Branch(label, Vector(other))

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
    renderState(tree).lines.mkString("\n")

  private def isSuccess(tree: Tree): Boolean =
    @tailrec
    def loop(todo: List[Tree]): Boolean =
      todo match
        case Nil =>
          true
        case Empty :: rest =>
          loop(rest)
        case Leaf(_, success) :: rest =>
          success && loop(rest)
        case Branch(_, children) :: rest =>
          loop(children.toList ::: rest)
        case Group(children) :: rest =>
          loop(children.toList ::: rest)

    loop(List(tree))

  private final case class RenderState(success: Boolean, lines: Vector[String])

  private enum Frame:
    case Visit(tree: Tree, depth: Int)
    case BuildBranch(label: String, depth: Int, childCount: Int)
    case BuildGroup(childCount: Int)

  private def renderState(tree: Tree): RenderState =
    @tailrec
    def loop(todo: List[Frame], done: List[RenderState]): RenderState =
      todo match
        case Nil =>
          done.headOption.getOrElse(RenderState(success = true, Vector.empty))
        case Frame.Visit(Empty, _) :: rest =>
          loop(rest, RenderState(success = true, Vector.empty) :: done)
        case Frame.Visit(Leaf(label, success), depth) :: rest =>
          loop(rest, RenderState(success, Vector(s"${indent(depth)}${show(label, success)}")) :: done)
        case Frame.Visit(Branch(label, children), depth) :: rest =>
          val visits = children.iterator.map(child => Frame.Visit(child, depth + 1)).toList
          loop(visits ::: Frame.BuildBranch(label, depth, children.size) :: rest, done)
        case Frame.Visit(Group(children), depth) :: rest =>
          val visits = children.iterator.map(child => Frame.Visit(child, depth)).toList
          loop(visits ::: Frame.BuildGroup(children.size) :: rest, done)
        case Frame.BuildBranch(label, depth, childCount) :: rest =>
          val (children, remaining) = done.splitAt(childCount)
          val orderedChildren = children.reverse
          val success         = orderedChildren.forall(_.success)
          val ownLine         = s"${indent(depth)}${show(label, success)}"
          val lines           = ownLine +: orderedChildren.flatMap(_.lines).toVector
          loop(rest, RenderState(success, lines) :: remaining)
        case Frame.BuildGroup(childCount) :: rest =>
          val (children, remaining) = done.splitAt(childCount)
          val orderedChildren = children.reverse
          val success         = orderedChildren.forall(_.success)
          val lines           = orderedChildren.flatMap(_.lines).toVector
          loop(rest, RenderState(success, lines) :: remaining)

    loop(List(Frame.Visit(tree, 0)), Nil)

  private def show(label: String, success: Boolean): String =
    if success then label else s"$label: Failed"

  private def indent(depth: Int): String =
    "  " * depth
