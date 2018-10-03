package fpinscala.errorhandling

import org.scalatest.{FlatSpecLike, Matchers}


class OptionTest extends FlatSpecLike with Matchers {

  "Option.flatMap" should "apply given function flattening the result for Some" in {
    Some(1).flatMap(i => Some(i.toString)) should be (Some("1"))
  }
  "Option.flatMap" should "return None for None" in {
    None.flatMap(i => Some(i.toString)) should be (None)
  }

  "Option.map" should "apply given function for Some" in {
    Some(1).map(_.toString) should be (Some("1"))
  }

  "Option.map" should "return None for None" in {
    None.map(_.toString) should be (None)
  }

  "Option.getOrElse" should "return the contained value" in {
    Some(1).getOrElse(2) should be (1)
  }

  "Option.getOrElse" should "return the default value for None" in {
    None.getOrElse(2) should be (2)
  }

  "Option.orElse" should "return itself when it's Some" in {
    Some(1).orElse(Some(2)) should be (Some(1))
  }

  "Option.orElse" should "return alternative when it's None" in {
    None.orElse(Some(2)) should be (Some(2))
  }

  "Option.filter" should "return itself when the predicate holds" in {
    Some(1).filter(_ % 2 == 1) should be (Some(1))
  }

  "Option.filter" should "return None when the predicate doesn't hold" in {
    Some(0).filter(_ % 2 == 1) should be (None)
  }

  for ((s, v) <- Seq(
    (Seq(1.0, 1.0, 1.0), Some(0.0)),
    (Seq(0.0, 3.0, 6.0), Some(6.0))
  )) {
    "Option.variance" should s"return $v for $s" in {
      Option.variance(s) should be (v)
    }

  }

  "Option.map2" should "apply the function if 2 given arguments are Some" in {
    Option.map2(Some(1), Some(2)) {
      (a, b) => a + ", " + b should be ("1, 2")
    }
  }

  it should "return None if the left argument is None" in {
    Option.map2(None: Option[Int], Some(2)) {
      (a, b) => a + ", " + b should be (None)
    }
  }

  it should "return None if the right argument is None" in {
    Option.map2(Some(2), None: Option[Int]) {
      (a, b) => a + ", " + b should be (None)
    }
  }

  it should "return None if both arguments are None" in {
    Option.map2(None: Option[Int], None: Option[Int]) {
      (a, b) => a + ", " + b should be (None)
    }
  }

  "Option.sequence" should "change a list of options to an option of list" in {
    Option.sequence(List(Some(1), Some(2), Some(3))) should be (Some(List(1, 2, 3)))
  }

  it should "return None if any element of list is None" in {
    Option.sequence(List(Some(1), None, Some(3))) should be (None)
  }

  "Option.traverse" should "combine results of each flatMapping" in {
    Option.traverse(List(1, 3, 5))(a => if(a % 2 == 1) Some(a) else None) should be (Some(List(1, 3, 5)))
  }

  it should "combine results of each flatMapping returning None if any of results is None" in {
    Option.traverse(List(1, 2, 3))(a => if(a % 2 == 1) Some(a) else None) should be (None)
  }

  "Option.sequenceViaTraverse" should "change a list of options to an option of list" in {
    Option.sequenceViaTraverse(List(Some(1), Some(2), Some(3))) should be (Some(List(1, 2, 3)))
  }

  it should "return None if any element of list is None" in {
    Option.sequenceViaTraverse(List(Some(1), None, Some(3))) should be (None)
  }


}
