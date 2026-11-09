package treelog.direct.cats

import _root_.cats.{Eq, Id}
import _root_.cats.instances.all.*
import _root_.cats.laws.discipline.{FunctorTests, MonadTests}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import treelog.direct.{Logged, Tree}

class LoggedTLawsSpec extends munit.DisciplineSuite:
  private type LawF[A] = LoggedT[Id, String, A]

  private val functor = LoggedT.loggedTFunctor[Id, String]
  private val monad   = LoggedT.loggedTMonad[Id, String]

  private given [A: Eq]: Eq[LawF[A]] =
    Eq.instance: (left, right) =>
      Eq[Either[String, A]].eqv(left.value.result, right.value.result) &&
        left.value.tree == right.value.tree

  private def withBranch(tree: Tree): Gen[Tree] =
    Gen
      .option(Gen.alphaStr)
      .map:
        case Some(label) => Tree.branch(label, tree)
        case None        => tree

  private val successfulTree: Gen[Tree] =
    for
      size     <- Gen.choose(0, 4)
      children <- Gen.listOfN(size, Gen.alphaStr.flatMap(label => withBranch(Tree.leaf(label))))
      tree     <- withBranch(children.foldLeft(Tree.empty)(Tree.combine))
    yield tree

  private given [A: Arbitrary]: Arbitrary[LawF[A]] =
    Arbitrary:
      for
        history <- successfulTree
        logged  <- Gen.frequency(
                     3 -> Arbitrary.arbitrary[A].map(value => Logged[String, A](Right(value), history)),
                     2 -> (for
                       error <- Gen.alphaStr
                       label <- Gen.alphaStr
                       tree  <- withBranch(Tree.combine(history, Tree.leaf(label, success = false)))
                     yield Logged[String, A](Left(error), tree))
                   )
      yield LoggedT[Id, String, A](logged)

  checkAll(
    "LoggedT[Id, String, *].Monad",
    MonadTests[LawF](using monad).monad[Int, String, Long]
  )

  checkAll(
    "LoggedT[Id, String, *].Functor",
    FunctorTests[LawF](using functor).functor[Int, String, Long]
  )

  property("standalone Functor map agrees with Monad map"):
    forAll: (value: LawF[Int], f: Int => String) =>
      Eq[LawF[String]].eqv(functor.map(value)(f), monad.map(value)(f))

  test("law equality observes results and tree structure"):
    val equality = Eq[LawF[Int]]
    val one      = LoggedT.success[Id, String, Int](1, "one")
    val two      = LoggedT.success[Id, String, Int](2, "one")
    val renamed  = LoggedT.success[Id, String, Int](1, "other")
    val nested   = LoggedT.branch[Id, String, Int]("parent")(one)
    val flat     = LoggedT[Id, String, Int](
      Logged(Right(1), Tree.combine(Tree.leaf("parent"), Tree.leaf("  one")))
    )
    val failed   = LoggedT.failure[Id, String, Int]("error", "failed")
    val other    = LoggedT.failure[Id, String, Int]("other error", "failed")

    assert(equality.eqv(one, LoggedT.success[Id, String, Int](1, "one")))
    assert(!equality.eqv(one, two))
    assert(!equality.eqv(one, renamed))
    assertEquals(nested.value.tree.render, flat.value.tree.render)
    assert(!equality.eqv(nested, flat))
    assert(!equality.eqv(failed, other))
