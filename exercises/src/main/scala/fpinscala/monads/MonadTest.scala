package fpinscala.monads

import org.scalatest.{FunSuite, Matchers}

class MonadTest extends FunSuite with Matchers{

  import Monad._

  test("sequence list") {
    optionMonad.sequence(List(Some(1), None, Some(2))) should be(None)
    optionMonad.sequence(List(Some(1), Some(2))) should be(Some(List(1, 2)))
  }

  test("traverse") {
    optionMonad.traverse(List(1,2,3))(Some(_)) should be (Option(List(1,2,3)))
  }

  test("filterM") {
    optionMonad.filterM(List(1,2,3,4))(Some(_) map (_ % 2 == 0)) should
      be (Option(List(2,4)))
  }

  test("flatMap via compose") {
    optionMonad._flatMap(Some(1))(i => Some(i * 2)) should be(Some(2))
    optionMonad._flatMap(Some(1))(_ => None) should be(None)
    optionMonad._flatMap(None: Option[Int])(i => Some(i * 2)) should be(None)
  }

}
