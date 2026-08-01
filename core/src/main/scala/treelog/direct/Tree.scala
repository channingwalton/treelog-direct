package treelog.direct

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

sealed trait Tree extends Product with Serializable:
  private[direct] def successful: Boolean

  final def isSuccess: Boolean =
    successful

  final def render: String =
    Tree.render(this)

  final override def equals(other: Any): Boolean =
    Tree.same(this, other)

  final override def hashCode(): Int =
    Tree.hash(this)

  final override def toString: String =
    Tree.describe(this)

object Tree:
  final private class EmptyTree extends Tree:
    val successful: Boolean = true

    def canEqual(that: Any): Boolean                    = that.isInstanceOf[EmptyTree]
    def productArity: Int                               = 0
    def productElement(index: Int): Any                 =
      throw new IndexOutOfBoundsException(index.toString)
    override def productElementName(index: Int): String =
      throw new IndexOutOfBoundsException(index.toString)
    override def productPrefix: String                  = "Empty"

  private val Empty = new EmptyTree

  final private class Leaf(val label: String, val successful: Boolean) extends Tree:
    def canEqual(that: Any): Boolean                    = that.isInstanceOf[Leaf]
    def productArity: Int                               = 2
    def productElement(index: Int): Any                 =
      index match
        case 0 => label
        case 1 => successful
        case _ => throw new IndexOutOfBoundsException(index.toString)
    override def productElementName(index: Int): String =
      index match
        case 0 => "label"
        case 1 => "success"
        case _ => throw new IndexOutOfBoundsException(index.toString)
    override def productPrefix: String                  = "Leaf"

  final private class Branch(
    val label: String,
    val children: Vector[Tree],
    val successful: Boolean
  ) extends Tree:
    def canEqual(that: Any): Boolean                    = that.isInstanceOf[Branch]
    def productArity: Int                               = 2
    def productElement(index: Int): Any                 =
      index match
        case 0 => label
        case 1 => children
        case _ => throw new IndexOutOfBoundsException(index.toString)
    override def productElementName(index: Int): String =
      index match
        case 0 => "label"
        case 1 => "children"
        case _ => throw new IndexOutOfBoundsException(index.toString)
    override def productPrefix: String                  = "Branch"

  final private class Group(
    val children: Vector[Tree],
    val successful: Boolean
  ) extends Tree:
    def canEqual(that: Any): Boolean                    = that.isInstanceOf[Group]
    def productArity: Int                               = 1
    def productElement(index: Int): Any                 =
      index match
        case 0 => children
        case _ => throw new IndexOutOfBoundsException(index.toString)
    override def productElementName(index: Int): String =
      index match
        case 0 => "children"
        case _ => throw new IndexOutOfBoundsException(index.toString)
    override def productPrefix: String                  = "Group"

  val empty: Tree =
    Empty

  def leaf(label: String, success: Boolean = true): Tree =
    new Leaf(label, success)

  def branch(label: String, child: Tree): Tree =
    child match
      case _: EmptyTree => Empty
      case group: Group => new Branch(label, group.children, group.successful)
      case other        => new Branch(label, Vector(other), other.successful)

  def combine(left: Tree, right: Tree): Tree =
    (left, right) match
      case (_: EmptyTree, r)           => r
      case (l, _: EmptyTree)           => l
      case (left: Group, right: Group) =>
        new Group(left.children ++ right.children, left.successful && right.successful)
      case (left: Group, r)            =>
        new Group(left.children :+ r, left.successful && r.successful)
      case (l, right: Group)           =>
        new Group(l +: right.children, l.successful && right.successful)
      case (l, r)                      =>
        new Group(Vector(l, r), l.successful && r.successful)

  private def render(tree: Tree): String =
    renderState(tree).lines.mkString("\n")

  private def same(left: Tree, other: Any): Boolean =
    other match
      case right: Tree =>
        @tailrec
        def loop(todo: List[(Tree, Tree)]): Boolean =
          todo match
            case Nil                                                                => true
            case (l, r) :: rest if l.asInstanceOf[AnyRef] eq r.asInstanceOf[AnyRef] =>
              loop(rest)
            case (_: EmptyTree, _: EmptyTree) :: rest                               => loop(rest)
            case (l: Leaf, r: Leaf) :: rest                                         =>
              l.label == r.label && l.successful == r.successful && loop(rest)
            case (l: Branch, r: Branch) :: rest                                     =>
              if l.label == r.label && l.children.size == r.children.size then
                loop(l.children.iterator.zip(r.children.iterator).toList ::: rest)
              else false
            case (l: Group, r: Group) :: rest                                       =>
              if l.children.size == r.children.size then loop(l.children.iterator.zip(r.children.iterator).toList ::: rest)
              else false
            case _                                                                  => false

        loop(List((left, right)))
      case _           => false

  private def hash(tree: Tree): Int =
    @tailrec
    def loop(todo: List[Tree], current: Int, values: Int): Int =
      todo match
        case Nil                      =>
          MurmurHash3.finalizeHash(current, values)
        case (_: EmptyTree) :: rest   =>
          loop(rest, MurmurHash3.mix(current, 0), values + 1)
        case (leaf: Leaf) :: rest     =>
          val withType    = MurmurHash3.mix(current, 1)
          val withLabel   = MurmurHash3.mix(withType, labelHash(leaf.label))
          val withSuccess = MurmurHash3.mix(withLabel, leaf.successful.##)
          loop(rest, withSuccess, values + 3)
        case (branch: Branch) :: rest =>
          val withType     = MurmurHash3.mix(current, 2)
          val withLabel    = MurmurHash3.mix(withType, labelHash(branch.label))
          val withChildren = MurmurHash3.mix(withLabel, branch.children.size)
          loop(branch.children.toList ::: rest, withChildren, values + 3)
        case (group: Group) :: rest   =>
          val withType     = MurmurHash3.mix(current, 3)
          val withChildren = MurmurHash3.mix(withType, group.children.size)
          loop(group.children.toList ::: rest, withChildren, values + 2)

    loop(List(tree), MurmurHash3.productSeed, 0)

  private def labelHash(label: String): Int =
    if label == null then 0 else label.hashCode

  private enum DescriptionFrame:
    case Visit(tree: Tree)
    case Text(value: String)

  private def describe(tree: Tree): String =
    val output = new StringBuilder

    @tailrec
    def loop(todo: List[DescriptionFrame]): String =
      todo match
        case Nil                                            => output.result()
        case DescriptionFrame.Text(value) :: rest           =>
          val _ = output.append(value)
          loop(rest)
        case DescriptionFrame.Visit(_: EmptyTree) :: rest   =>
          val _ = output.append("Empty")
          loop(rest)
        case DescriptionFrame.Visit(leaf: Leaf) :: rest     =>
          val _ = output
            .append("Leaf(")
            .append(String.valueOf(leaf.label))
            .append(",")
            .append(leaf.successful)
            .append(")")
          loop(rest)
        case DescriptionFrame.Visit(branch: Branch) :: rest =>
          val _ = output
            .append("Branch(")
            .append(String.valueOf(branch.label))
            .append(",Vector(")
          loop(descriptionFrames(branch.children, "))") ::: rest)
        case DescriptionFrame.Visit(group: Group) :: rest   =>
          val _ = output.append("Group(Vector(")
          loop(descriptionFrames(group.children, "))") ::: rest)

    loop(List(DescriptionFrame.Visit(tree)))

  private def descriptionFrames(children: Vector[Tree], closing: String): List[DescriptionFrame] =
    val frames = List.newBuilder[DescriptionFrame]
    children.iterator.zipWithIndex.foreach: (child, index) =>
      if index > 0 then frames += DescriptionFrame.Text(",")
      frames += DescriptionFrame.Visit(child)
    frames += DescriptionFrame.Text(closing)
    frames.result()

  final private case class RenderState(success: Boolean, lines: Vector[String])

  private enum Frame:
    case Visit(tree: Tree, depth: Int)
    case BuildBranch(label: String, depth: Int, childCount: Int)
    case BuildGroup(childCount: Int)

  private def renderState(tree: Tree): RenderState =
    @tailrec
    def loop(todo: List[Frame], done: List[RenderState]): RenderState =
      todo match
        case Nil                                                 =>
          done.headOption.getOrElse(RenderState(success = true, Vector.empty))
        case Frame.Visit(_: EmptyTree, _) :: rest                =>
          loop(rest, RenderState(success = true, Vector.empty) :: done)
        case Frame.Visit(leaf: Leaf, depth) :: rest              =>
          loop(rest, RenderState(leaf.successful, show(leaf.label, leaf.successful, depth)) :: done)
        case Frame.Visit(branch: Branch, depth) :: rest          =>
          val visits = branch.children.iterator.map(child => Frame.Visit(child, depth + 1)).toList
          loop(visits ::: Frame.BuildBranch(branch.label, depth, branch.children.size) :: rest, done)
        case Frame.Visit(group: Group, depth) :: rest            =>
          val visits = group.children.iterator.map(child => Frame.Visit(child, depth)).toList
          loop(visits ::: Frame.BuildGroup(group.children.size) :: rest, done)
        case Frame.BuildBranch(label, depth, childCount) :: rest =>
          val (children, remaining) = done.splitAt(childCount)
          val orderedChildren       = children.reverse
          val success               = orderedChildren.forall(_.success)
          val lines                 = show(label, success, depth) ++ orderedChildren.flatMap(_.lines)
          loop(rest, RenderState(success, lines) :: remaining)
        case Frame.BuildGroup(childCount) :: rest                =>
          val (children, remaining) = done.splitAt(childCount)
          val orderedChildren       = children.reverse
          val success               = orderedChildren.forall(_.success)
          val lines                 = orderedChildren.flatMap(_.lines).toVector
          loop(rest, RenderState(success, lines) :: remaining)

    loop(List(Frame.Visit(tree, 0)), Nil)

  private def show(label: String, success: Boolean, depth: Int): Vector[String] =
    val text   = if success then String.valueOf(label) else s"${String.valueOf(label)}: Failed"
    val prefix = indent(depth)
    text.split("\\R", -1).iterator.map(line => s"$prefix$line").toVector

  private def indent(depth: Int): String =
    "  " * depth
