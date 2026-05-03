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
