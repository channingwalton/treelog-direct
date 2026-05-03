package treelog.direct

enum Tree:
  case Empty
  case Leaf(label: String, success: Boolean)
  case Branch(label: Option[String], success: Boolean, children: Vector[Tree])

  def isSuccess: Boolean =
    this match
      case Tree.Empty                 => true
      case Tree.Leaf(_, success)      => success
      case Tree.Branch(_, success, _) => success

  def render: String =
    Tree.render(this)

object Tree:
  def leaf(label: String, success: Boolean = true): Tree =
    Tree.Leaf(label, success)

  def branch(label: String, child: Tree): Tree =
    val children = child match
      case Tree.Empty                         => Vector.empty
      case Tree.Branch(None, _, grandchildren) => grandchildren
      case other                              => Vector(other)

    Tree.Branch(Some(label), child.isSuccess, children)

  def combine(left: Tree, right: Tree): Tree =
    (left, right) match
      case (Tree.Empty, r) => r
      case (l, Tree.Empty) => l
      case (Tree.Branch(None, ls, lc), Tree.Branch(None, rs, rc)) =>
        Tree.Branch(None, ls && rs, lc ++ rc)
      case (Tree.Branch(None, ls, lc), r) =>
        Tree.Branch(None, ls && r.isSuccess, lc :+ r)
      case (l, Tree.Branch(None, rs, rc)) =>
        Tree.Branch(None, l.isSuccess && rs, l +: rc)
      case (l, r) =>
        Tree.Branch(None, l.isSuccess && r.isSuccess, Vector(l, r))

  private def render(tree: Tree): String =
    lines(tree, 0).mkString(System.lineSeparator)

  private def lines(tree: Tree, depth: Int): Vector[String] =
    tree match
      case Tree.Empty =>
        Vector.empty
      case Tree.Leaf(label, success) =>
        Vector(s"${indent(depth)}${show(label, success)}")
      case Tree.Branch(label, success, children) =>
        val ownLine = label.toVector.map(value => s"${indent(depth)}${show(value, success)}")
        ownLine ++ children.flatMap(child => lines(child, depth + ownLine.size))

  private def show(label: String, success: Boolean): String =
    if success then label else s"$label: Failed"

  private def indent(depth: Int): String =
    "  " * depth
