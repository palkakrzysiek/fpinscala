package fpinscala.monads

import fpinscala.state.State
import org.scalatest.{FunSuite, Matchers}

class MonadTest extends FunSuite with Matchers {

  import Monad._

  test("sequence list") {
    optionMonad.sequence(List(Some(1), None, Some(2))) should be(None)
    optionMonad.sequence(List(Some(1), Some(2))) should be(Some(List(1, 2)))
  }

  test("traverse") {
    optionMonad.traverse(List(1, 2, 3))(Some(_)) should be(Option(List(1, 2, 3)))
  }

  test("filterM") {
    optionMonad.filterM(List(1, 2, 3, 4))(Some(_) map (_ % 2 == 0)) should
      be(Option(List(2, 4)))
  }

  test("flatMap via compose") {
    optionMonad._flatMap(Some(1))(i => Some(i * 2)) should be(Some(2))
    optionMonad._flatMap(Some(1))(_ => None) should be(None)
    optionMonad._flatMap(None: Option[Int])(i => Some(i * 2)) should be(None)
  }

  test("flatMap via join") {
    optionMonad.__flatMap(Some(1))(i => Some(i * 2)) should be(Some(2))
    optionMonad.__flatMap(Some(1))(_ => None) should be(None)
    optionMonad.__flatMap(None: Option[Int])(i => Some(i * 2)) should be(None)
  }

  test("state monad") {
    //    val increase = stateMonad[Int].flatMap(stateMonad.unit(()))(i => State(i => (i+1, i+1)))
    val increase = State((i: Int) => (i + 1, i + 1))
    stateMonad.replicateM(3, increase).run(1) should be ((List(4, 3, 2), 4))
    stateMonad.sequence(List.fill(3)(increase)).run(1) should be ((List(4, 3, 2), 4))
    stateMonad.map2(increase, increase)(("mapped", _, _)).run(1) should be ((("mapped", 2, 3), 3))


  }
}
