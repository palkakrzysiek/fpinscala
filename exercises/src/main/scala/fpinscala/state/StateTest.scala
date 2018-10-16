package fpinscala.state

import org.scalatest.{FunSuite, Matchers}

class StateTest extends FunSuite with Matchers {
  test("State.flatMap") {
    val s1 =State[Int, String](i => (s"State: $i", i + 1))
    val s2 = s1.flatMap(str => State[Int, String](i => (s"OldVal: $str, " +
      s"oldState: $i", i *2 )))
    s2.run(1) should be (("OldVal: State: 1, oldState: 2", 4) )
  }

  test("State for-comprehension") {
    val increase =State[Int, Int](i => (i, i + 1))
    val increaseIsEven = increase.map(_ % 2 == 0)
    val res: State[Int, (Int, Boolean, Boolean, Int)] = for {
      i1 <- increase
      b1 <- increaseIsEven
      b2 <- increaseIsEven
      i2 <- increase
    } yield (i1, b1, b2, i2)

    res.run(0) should be ((0, false, true, 3), 4)
  }

  test("State.map") {
    val s1 =State[Int, String](i => (s"State: $i", i + 1))
    val s2 = s1.map(_.toUpperCase)
    s2.run(1) should be (("STATE: 1", 2))
    val s3 = s2.map(_.toLowerCase)
    s3.run(1) should be (("state: 1", 2))
  }

  test("map2") {
    val increase =State[Int, Int](i => (i, i + 1))
    val increaseIsEven = increase.map(_ % 2 == 0)
    val res = increase.map2(increaseIsEven)((_, _))

    res.run(0) should be ((0, false), 2)
  }

  test("sequence") {
    val increase =State[Int, Int](i => (i, i + 1))
    val res = State.sequence(List.fill(3)(increase))

    res.run(0) should be (List(0, 1, 2), 3)
  }
}
