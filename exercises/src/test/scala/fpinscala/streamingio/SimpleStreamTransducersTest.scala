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
}
