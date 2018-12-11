package fpinscala.applicative

import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable

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
}
