package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {

  import List._

  "List.tail" should "return the tail of the list" in {
    tail(List("a", "b", "c")) should be(List("b", "c"))
  }

  it should "return Nil for a single-element list" in {
    tail(List("a")) should be(Nil)
  }

  it should "return NoSuchElementException for an empty list" in {
    assertThrows[NoSuchElementException] {
      tail(List())
    }
  }

  "List.setHead" should "prepend an element to the list" in {
    setHead(Nil, "abc") should be(List("abc"))
    setHead(List("abc"), "xyz") should be(List("xyz", "abc"))
  }

  "List.drop" should "remove the first n elements form the list" in {
    drop(List("a", "b", "c"), 2) should be(List("c"))
    drop(List("a", "b", "c"), 3) should be(Nil)
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
    dropWhile(List(1, 2, 3, 4), (i: Int) => i < 3) should be(List(3, 4))
    dropWhile(Nil, (i: Int) => i < 3) should be(Nil)
  }

  "List.init" should "drop the last element of the list" in {
    init(List("a", "b", "c")) should be(List("a", "b"))
    init(List("a")) should be(Nil)
    init(Nil) should be(Nil)
  }

  "List.foldRight" should "be able to reconstruct the list" in {
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) should be(List(1, 2, 3))
  }

  "List.length" should "return the length of the list" in {
    List.length(List()) should be(0)
    List.length(List(1, 2, 3)) should be(3)
  }

  "List.foldLeft" should "apply an operation while folding the list" in {
    foldLeft(List(1.0, 2.0, 3.0), Nil: List[Double]) { (l, e) =>
      Cons(e, l)
    } should be(List(3.0, 2.0, 1.0))
  }

  "List.append2" should "append the second list to the first" in {
    append2(List(1, 2), List(3, 4)) should be(List(1, 2, 3, 4))
  }

  "List.flatten" should "make a single list from a list of lists" in {
    flatten(List(List(1, 2), List(3, 4), List(5, 6))) should be(List(1, 2, 3, 4, 5, 6))
  }

  "List.add1" should "add 1 to each integer of the list" in {
    add1(List(0, 1, 2)) should be(List(1, 2, 3))
  }

  "List.d2s" should "convert a list of doubles to a list of strings" in {
    d2s(List(1.0, 2.0, 3.0)) should be(List("1.0", "2.0", "3.0"))
  }

  "List.map" should "apply a function on each element of the list" in {
    map(List(1.0, 2.0, 3.0))(_.toString) should be(List("1.0", "2.0", "3.0"))
  }

  "List.filter" should "remove all the elements not matching the predicate" in {
    filter(List(1, 2, 3, 4))(_ % 2 == 0) should be(List(2, 4))
  }

  "List.flatMap" should "flatten the results of function given as the argument" in {
    flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))
  }

  "List.filterUsingFlatMap" should "remove all the elements not matching the predicate" in {
    filterUsingFlatMap(List(1, 2, 3, 4))(_ % 2 == 0) should be(List(2, 4))
  }

  "List.addElements" should "add corresponding elements of 2 lists to each other" in {
    addElements(List(1, 2, 3), List(4, 5, 6)) should be(List(5, 7, 9))
  }

  "List.zipWith" should "apply a given function on subsequent elements of 2 given lists" in {
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) should be(List(5, 7, 9))
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ * _) should be(List(4, 10, 18))
  }

  val sup = List("a", "b", "c")
  for ((sub, res) <- Seq(
    (List(), true),
    (List("a"), true),
    (List("a", "b"), true),
    (List("a", "b", "c"), true),
    (List("b", "c"), true),
    (List("a", "b", "c", "d"), false),
    (List("d"), false)
  )) {
    "List.hasSubsequence" should s"return $res if $sup contains $sub preserving the order" in {
      hasSubsequence(sup, sub) should be(res)
    }

  }


}
