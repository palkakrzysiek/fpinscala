package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {

  import List._

  "List.tail" should "return the tail of the list" in {
    tail(List("a", "b", "c")) should be (List("b", "c"))
  }

  it should "return Nil for a single-element list" in {
    tail(List("a")) should be (Nil)
  }

  it should "return NoSuchElementException for an empty list" in {
    assertThrows[NoSuchElementException] {
      tail(List())
    }
  }

}
