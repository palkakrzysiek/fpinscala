package fpinscala.errorhandling

import org.scalatest.{FlatSpec, FunSuite, FunSuiteLike, Matchers}

import scala.{Either => _, Option => _}

class EitherTest extends FlatSpec with Matchers {

  "Either.map" should "map the right value" in {
    Right(1).map(_ + 1) should be (Right(2))
  }

  it should "leave the left unchanged" in {
    (Left("some error"): Either[String, Int]).map(_ + 1) should be (Left("some error"))
  }

  "Either.flatMap" should "map and flatten if it's Right" in {
    Right(1).flatMap(a => Right(a + 1)) should be (Right(2))
  }

  it should "leave the Left unchanged" in {
    (Left("some error"): Either[String, Int]).flatMap(a => Right(a + 1)) should be (Left("some error"))
  }

  "Either.orElse" should "return the alternative value if Left" in {
    Left("a").orElse(Left("b")) should be (Left("b"))
    Left("a").orElse(Right("b")) should be (Right("b"))
  }

  "Either.orElse" should "return the original value if Right" in {
    Right("a").orElse(Left("b")) should be (Right("a"))
    Right("a").orElse(Right("b")) should be (Right("a"))
  }

  "Either.map2" should "map if both arguments are Right" in {
    Right("a").map2(Right("b"))(_ + _) should be (Right("ab"))
  }

  it should "remain Left" in {
    (Left("a"): Either[String, String]).map2(Right("b"))(_ + _) should be (Left("a"))
  }

  it should "become Left if the second argument is Left" in {
    Right("b").map2(Left("a"): Either[String, String])(_ + _) should be (Left("a"))
  }


  "Either.traverse" should "apply function on the given list returning Right if every" in {
    List()
  }


}
