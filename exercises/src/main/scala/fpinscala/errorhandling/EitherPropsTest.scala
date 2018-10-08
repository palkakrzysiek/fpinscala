package fpinscala.errorhandling

import org.scalatest.{FunSuite, Matchers}


class EitherPropsTest extends FunSuite with Matchers{

  test("Either.traverse") {
    Either.traverse(List(2, 4, 6))(x => if (x % 2 == 1) Left(x) else Right(x)) should be (Right(List(2, 4, 6)))
    Either.traverse(List(0, 1, 2))(x => if (x % 2 == 1) Left(x) else Right(x)) should be (Left(1))
  }

  test("Either.sequence") {
    Either.sequence(List(Right(1),Right(2))) should be (Right(List(1, 2)))
    Either.sequence(List(Right(1),Left(2))) should be (Left(2))
  }

}
