package fpinscala.applicative

import org.scalatest.{FunSuite, Matchers}
import org.scalacheck.Prop.{forAll, BooleanOperators}
import Applicative._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import Arbitrary.arbitrary
import org.scalatest.prop.Checkers



class ApplicativeTest extends FunSuite with Matchers with Checkers {
  test("stream applicative") {
    val sequenced: Stream[List[Int]] = streamApplicative
      .sequence(List(
        Stream(1, 2, 3),
        Stream(4, 5, 6),
        Stream.continually(0)
      ))
    sequenced(0) should be(List(1, 4, 0))
    sequenced(1) should be(List(2, 5, 0))
    sequenced(2) should be(List(3, 6, 0))
    an [IndexOutOfBoundsException] should be thrownBy sequenced(3)
  }

  test("list traverse") {
    Traverse.listTraverse.traverse(List(1,2,3))(i => Stream.continually(i))(streamApplicative).take(2) should be (Stream(List(1, 2, 3), List(1, 2, 3)))
  }

  test("option traverse") {
    Traverse.optionTraverse.traverse(Some(1))(i => Stream.continually(i))(streamApplicative).take(2) should be (Stream(Some(1), Some(1)))
    Traverse.optionTraverse.traverse(None)(i => Stream.continually(i))(streamApplicative).take(2) should be (Stream(None, None))
  }

  test("traverse reverse") {
    import Traverse._
    implicit def arbTree[T](implicit a: Arbitrary[T]): Arbitrary[Tree[T]] = Arbitrary {
      def newTree(maxDeph: Int): Gen[Tree[T]] = for {
        head <- a.arbitrary
        nodesSize <- Gen.choose(0, maxDeph)
        nodes <- Gen.listOfN(nodesSize, newTree(maxDeph - 1))
      } yield Tree(head, nodes)
      newTree(6)
    }

    check { (x: Tree[Int], y: Tree[Int]) => {
      x.reverse.toList ++ y.reverse.toList == (y.toList ++ x.toList).reverse
    }
    }
  }

  test("foldLeft via mapAccum") {
    import Traverse.listTraverse

    listTraverse.foldLeft(List(3,2))(6)(_ / _) should be (1)
  }
}
