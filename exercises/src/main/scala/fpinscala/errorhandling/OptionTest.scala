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


}
