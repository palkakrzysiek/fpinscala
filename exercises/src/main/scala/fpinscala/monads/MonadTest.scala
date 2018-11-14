package fpinscala.monads

import org.scalatest.{FunSuite, Matchers}

class MonadTest extends FunSuite with Matchers{

  import Monad._

  test("sequence list") {
    optionMonad.sequence(List(Some(1), None, Some(2))) should be(None)
    optionMonad.sequence(List(Some(1), Some(2))) should be(Some(List(1, 2)))
  }

}
