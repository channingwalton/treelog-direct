package treelog.direct

class TreeSpec extends munit.FunSuite:
  test("combines trees without rendering an anonymous wrapper"):
    val tree =
      Tree.combine(
        Tree.branch("First", Tree.leaf("one")),
        Tree.branch("Second", Tree.leaf("two"))
      )

    assertEquals(
      tree.render,
      """First
        |  one
        |Second
        |  two""".stripMargin
    )

  test("derives branch failure from failed children"):
    val tree =
      Tree.branch(
        "Parent",
        Tree.combine(
          Tree.leaf("success"),
          Tree.leaf("failure", success = false)
        )
      )

    assertEquals(tree.isSuccess, false)
    assertEquals(
      tree.render,
      """Parent: Failed
        |  success
        |  failure: Failed""".stripMargin
    )

  test("renders with line feed separators"):
    val tree =
      Tree.combine(Tree.leaf("one"), Tree.leaf("two"))

    assertEquals(tree.render, "one\ntwo")

  test("indents every line of multiline labels"):
    val tree =
      Tree.branch("parent", Tree.leaf("first\nsecond", success = false))

    assertEquals(
      tree.render,
      """parent: Failed
        |  first
        |  second: Failed""".stripMargin
    )

  test("normalizes and indents multiline branch labels"):
    val tree =
      Tree.branch("parent\r\ncontext", Tree.leaf("first\u2028second", success = false))

    assertEquals(
      tree.render,
      """parent
        |context: Failed
        |  first
        |  second: Failed""".stripMargin
    )

  test("skips empty branches"):
    val tree =
      Tree.combine(
        Tree.leaf("one"),
        Tree.branch("Empty", Tree.empty)
      )

    assertEquals(tree.render, "one")
    assertEquals(tree.isSuccess, true)

  test("renders deep trees without overflowing the stack"):
    val tree =
      (1 to 10000).foldLeft(Tree.leaf("leaf")): (child, depth) =>
        Tree.branch(s"branch $depth", child)

    assertEquals(tree.isSuccess, true)
    assert(tree.render.startsWith("branch 10000\n  branch 9999"))

  test("preserves structural equality and descriptions"):
    val leaf = Tree.leaf("leaf")

    assertEquals(leaf, Tree.leaf("leaf"))
    assertNotEquals(leaf, Tree.leaf("other"))
    assertNotEquals(leaf, Tree.leaf("leaf", success = false))
    assertNotEquals(Tree.branch("one", leaf), Tree.branch("two", leaf))
    assertNotEquals(Tree.branch("one", leaf), Tree.combine(Tree.leaf("one"), leaf))
    assertEquals(Tree.empty.toString, "Empty")
    assertEquals(Tree.branch("parent", leaf).toString, "Branch(parent,Vector(Leaf(leaf,true)))")
    assertEquals(
      Tree.combine(Tree.branch("parent", leaf), Tree.leaf("other", success = false)).toString,
      "Group(Vector(Branch(parent,Vector(Leaf(leaf,true))),Leaf(other,false)))"
    )

    val runtimeLeaf: Any = leaf
    assert(runtimeLeaf.isInstanceOf[Product])
    assert(runtimeLeaf.isInstanceOf[java.io.Serializable])
    assertEquals(leaf.productElementNames.toList, List("label", "success"))
    assertEquals(leaf.productIterator.toList, List("leaf", true))

  test("handles null labels consistently"):
    val left  = Tree.leaf(null)
    val right = Tree.leaf(null)

    assertEquals(left, right)
    assertEquals(left.hashCode, right.hashCode)
    assertEquals(left.render, "null")
    assertEquals(left.toString, "Leaf(null,true)")

  test("compares, hashes, and describes deep trees without overflowing the stack"):
    def deepTree(leaf: String): Tree =
      (1 to 10000).foldLeft(Tree.leaf(leaf)): (child, depth) =>
        Tree.branch(s"branch $depth", child)

    val left      = deepTree("leaf")
    val right     = deepTree("leaf")
    val different = deepTree("other")

    assertEquals(left, right)
    assertNotEquals(left, different)
    assertEquals(left.hashCode, right.hashCode)
    assert(left.toString.startsWith("Branch(branch 10000,Vector("))
