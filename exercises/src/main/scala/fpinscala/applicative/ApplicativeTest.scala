package fpinscala.applicative

import org.scalatest.{FunSuite, Matchers}

import org.scalacheck.Prop.forAll

import Applicative._

class ApplicativeTest extends FunSuite with Matchers {
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
    // TODO wirte a Tree[T] generator
    forAll { (x: List[Int], y: List[Int]) =>
      Tree(1, x).reverse.toList
    }
  }
}
