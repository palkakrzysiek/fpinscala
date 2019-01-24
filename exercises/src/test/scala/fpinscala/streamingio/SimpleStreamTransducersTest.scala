package fpinscala.streamingio

import org.scalatest.{FunSuite, Matchers}

class SimpleStreamTransducersTest extends FunSuite with Matchers {
  import SimpleStreamTransducers.Process._

  test("take") {
    take(3)(Stream(1,2,3,4,5)).toList should be (List(1,2,3))
    take(3)(Stream(1,2)).toList should be (List(1,2))
  }

  test("drop") {
    drop(3)(Stream(1,2,3,4,5)).toList should be (List(4,5))
    drop(3)(Stream(1,2)).toList should be (List.empty)
  }

  test("takeWhile") {
    takeWhile((i: Int) => i <= 3)(Stream(1,2,3,4,5)).toList should be (List(1,2,3))
    takeWhile((i: Int) => i <= 3)(Stream(4,5)).toList should be (List.empty)
  }

  test("dropWhile") {
    dropWhile((i: Int) => i <= 3)(Stream(1,2,3,4,5)).toList should be (List(4,5))
    dropWhile((i: Int) => i <= 3)(Stream(4,5)).toList should be (List(4,5))
    dropWhile((i: Int) => i <= 3)(Stream(1,2,3)).toList should be (List.empty)
     }

  test("count") {
    count(Stream("a", "b", "c")).toList should be (List(0, 1, 2))
  }

  test("mean") {
    mean(Stream(1, 3, 5)).toList should be (List(1, 2, 3))
  }

  test("sum2") {
    sum2(Stream(1,2,3)).toList should be (List(1,3,6))
  }

  test("count3") {
    count3(Stream("", "", "")).toList should be (List(1, 2, 3))
  }

  test("++") {
    val p1 = liftOne((i: Int) => i * 2).repeatN(2)
    val p2 = liftOne((i: Int) => i * 3).repeatN(2)
    (p1 ++ p2)(Stream.fill(7)(1)).toList should be (List(2,2,2,3,3,3))
  }
}
