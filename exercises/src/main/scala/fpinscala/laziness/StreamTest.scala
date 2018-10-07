package fpinscala.laziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  import Stream._

  test("Stream.toList") {
    assert(Stream(1, 2, 3).toList === List(1, 2, 3))
  }

  test("Stream.take") {
    var zero = 0

    def effectfulZero = {
      zero = 1
      0
    }

    assert(cons(2, cons(1, cons(effectfulZero, empty))).take(2).toList ===
      List(2, 1), "takes only first n parameters")
    assert(zero === 0, "don't force thunk")
  }

  test("Stream.drop") {
    var zero = 0

    def effectfulTwo = {
      zero = 1
      2
    }

    assert(cons(effectfulTwo, cons(1, cons(0, empty))).drop(2).toList ===
      List(0), "drops n first parameters")
    assert(zero === 0, "don't force thunk")
  }

  test("Stream.takeWhile") {

    var zero = 0

    def effectfulZero = {
      zero = 1
      0
    }

    assert(cons(2, cons(1, cons(effectfulZero, empty))).takeWhile(_ > 1).toList
      ===
      List(2), "takes only first n parameters")
    assert(zero === 0, "don't force thunk")
  }

  test("Stream.forAll") {

    var zero = 0

    def effectfulZero = {
      zero = 1
      0
    }

    assert(cons(2, cons(1, cons(effectfulZero, empty)))
      .forAll(_ > 1) === false, "takes only first n parameters")
    assert(zero === 0, "don't force thunk")
    assert(Stream(1, 2, 3).forAll(_ > 0) === true)
  }

  test("Stream.takeWhileViaFoldRight") {

    var zero = 0

    def effectfulZero = {
      zero = 1
      0
    }

    assert(cons(2, cons(1, cons(effectfulZero, empty)))
      .takeWhileViaFoldRight(_ > 1).toList === List(2),
      "takes only first n parameters")
    assert(zero === 0, "don't force thunk")
  }

  test("Stream.headOption") {

    var zero = 0

    def effectfulZero = {
      zero = 1
      0
    }

    assert(cons(2, cons(1, cons(effectfulZero, empty)))
      .headOption === Some(2),
      "takes only first n parameters")
    assert(zero === 0, "don't force thunk")
    assert(empty.headOption.isEmpty)
  }

  test("Stream.map") {

    assert(Stream(1, 2, 3).map(_ + 1).toList === List(2, 3, 4))

    var zero = 0

    def effectfulZero = {
      zero = 1
      0
    }

    assert(cons(2, cons(1, cons(effectfulZero, empty)))
      .map(_ + 1)
      .headOption === Some(3))
    assert(zero === 0, "don't force thunk")
    assert(empty.headOption.isEmpty)
  }

  test("Stream.filter") {

    var zero = 0

    def effectfulZero = {
      zero = 1
      0
    }

    assert(cons(2, cons(1, cons(effectfulZero, empty)))
      .filter(_ % 2 == 1)
      .headOption === Some(1))
    assert(zero === 0, "don't force thunk")
    assert(empty.headOption.isEmpty)
  }

  test("Stream.append") {
    assert(Stream(1).append(Stream("A")).toList === List(1, "A"))
  }

  test("Stream.flatMap") {
    assert(Stream(1, 2)
      .flatMap(a => Stream(a.toString)).toList === List("1", "2"))
  }

  test("Stream.constant") {
    val x = "x"
    assert(Stream.constant(x).take(5).toList === List(x, x, x, x, x))
  }

  test("Stream.from") {
    assert(Stream.from(1).take(3).toList === List(1, 2, 3))
  }

  test("Stream.fibs") {
    assert(Stream.fibs().take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
  }

  test("Stream.unfold") {
    assert(Stream.unfold(0)(s => Some(s + 1, s + 10)).take(3).toList === List(1,
      11, 21))

    assert(Stream.unfold(0)(s =>
      if (s < 2) Some(s, s + 1) else None).take(3).toList === List(0, 1))
  }

  test("Stream.fibsViaUnfold") {
    assert(Stream.fibsViaUnfold.take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
  }

  test("Stream.fromViaUnfold") {
    assert(Stream.fromViaUnfold(1).take(3).toList === List(1, 2, 3))
  }

  test("Stream.constantViaUnfold") {
    val x = "x"
    assert(Stream.constantViaUnfold(x).take(5).toList === List(x, x, x, x, x))
  }

  test("Stream.onesViaUnfold") {
    assert(Stream.onesViaUnfold.take(3).toList === List(1, 1, 1))
  }
}
