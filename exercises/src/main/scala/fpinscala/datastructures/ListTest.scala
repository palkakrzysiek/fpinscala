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

  "List.setHead" should "prepend an element to the list" in {
    setHead(Nil, "abc") should be (List("abc"))
    setHead(List("abc"), "xyz") should be (List("xyz", "abc"))
  }

  "List.drop" should "remove the first n elements form the list" in {
    drop(List("a", "b", "c"), 2) should be (List("c"))
    drop(List("a", "b", "c"), 3) should be (Nil)
  }

  it should "throw an exception while trying to remove more elements that are in the list" in {
    assertThrows[IndexOutOfBoundsException] {
      drop(List("a", "b", "c"), 4)
    }
    assertThrows[IndexOutOfBoundsException] {
      drop(Nil, 1)
    }
  }

  "List.dropWhile" should "drop elements as long as the given predicate holds" in {
    dropWhile(List(1, 2, 3, 4), (i: Int) => i < 3) should be (List(3, 4))
    dropWhile(Nil, (i: Int) => i < 3) should be (Nil)
  }

  "List.init" should "drop the last element of the list" in {
    init(List("a", "b", "c")) should be (List("a", "b"))
    init(List("a")) should be (Nil)
    init(Nil) should be (Nil)
  }

  "List.foldRight" should "be able to reconstruct the list" in {
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) should be (List(1, 2, 3))
  }

  "List.length" should "return the length of the list" in {
    List.length(List()) should be (0)
    List.length(List(1, 2, 3)) should be (3)
  }

  "List.foldLeft" should "apply an operation while folding the list" in {
    foldLeft(List(1.0, 2.0, 3.0), Nil: List[Double]) { (l, e) =>
      Cons(e, l)
    } should be (List(3.0, 2.0, 1.0))
  }

}
