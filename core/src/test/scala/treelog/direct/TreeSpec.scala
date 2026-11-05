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

  test("folds into a structured report in left-to-right post-order"):
    enum Report:
      case Empty
      case Leaf(label: String, success: Boolean)
      case Branch(label: String, success: Boolean, children: Vector[Report])
      case Group(success: Boolean, children: Vector[Report])

    val tree   = Tree.combine(
      Tree.branch(
        "parent\r\ncontext",
        Tree.combine(
          Tree.leaf("one"),
          Tree.branch("nested", Tree.leaf(null, success = false))
        )
      ),
      Tree.leaf("tail")
    )
    val visits = Vector.newBuilder[String]
    val report = tree.fold[Report](
      empty = fail("a non-empty tree must not evaluate the empty handler"),
      leaf = (label, success) =>
        visits += s"leaf:$label"
        Report.Leaf(label, success)
      ,
      branch = (label, success, children) =>
        visits += s"branch:$label"
        Report.Branch(label, success, children)
      ,
      group = (success, children) =>
        visits += "group"
        Report.Group(success, children)
    )

    assertEquals(
      report,
      Report.Group(
        false,
        Vector(
          Report.Branch(
            "parent\r\ncontext",
            false,
            Vector(
              Report.Leaf("one", true),
              Report.Branch("nested", false, Vector(Report.Leaf(null, false)))
            )
          ),
          Report.Leaf("tail", true)
        )
      )
    )
    assertEquals(
      visits.result(),
      Vector("leaf:one", "leaf:null", "branch:nested", "branch:parent\r\ncontext", "leaf:tail", "group")
    )

  test("folds a normalized empty tree once without invoking other handlers"):
    var evaluations = 0
    val result      = Tree
      .branch("removed", Tree.combine(Tree.empty, Tree.empty))
      .fold[Int](
        empty = {
          evaluations += 1
          42
        },
        leaf = (_, _) => fail("unexpected leaf"),
        branch = (_, _, _) => fail("unexpected branch"),
        group = (_, _) => fail("unexpected group")
      )

    assertEquals(result, 42)
    assertEquals(evaluations, 1)

  test("folds deep trees without overflowing the stack"):
    val tree =
      (1 to 100000).foldLeft(Tree.leaf("leaf")): (child, _) =>
        Tree.branch("branch", child)

    val nodes = tree.fold[Int](
      empty = fail("unexpected empty tree"),
      leaf = (_, success) =>
        assert(success)
        1
      ,
      branch = (_, success, children) =>
        assert(success)
        children.head + 1
      ,
      group = (_, _) => fail("unexpected group")
    )

    assertEquals(nodes, 100001)

  test("folds wide normalized groups with ordered child results"):
    val tree =
      (1 to 10000).foldLeft(Tree.empty): (children, index) =>
        Tree.combine(children, Tree.combine(Tree.empty, Tree.leaf(index.toString)))

    val total = tree.fold[Int](
      empty = fail("unexpected empty tree"),
      leaf = (label, _) => label.toInt,
      branch = (_, _, _) => fail("unexpected branch"),
      group = (success, children) =>
        assert(success)
        assertEquals(children, (1 to 10000).toVector)
        children.sum
    )

    assertEquals(total, 50005000)

  test("propagates handler exceptions and stops before later siblings"):
    val problem = new IllegalStateException("report failed")
    val visits  = Vector.newBuilder[String]
    val tree    = Tree.combine(Tree.branch("parent", Tree.leaf("first")), Tree.leaf("later"))

    val thrown = intercept[IllegalStateException]:
      tree.fold[Int](
        empty = fail("unexpected empty tree"),
        leaf = (label, _) =>
          visits += label
          1
        ,
        branch = (_, _, _) => throw problem,
        group = (_, _) => fail("unexpected group")
      )

    assert(thrown eq problem)
    assertEquals(visits.result(), Vector("first"))
