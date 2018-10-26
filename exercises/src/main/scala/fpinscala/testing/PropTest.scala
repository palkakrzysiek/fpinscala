package fpinscala.testing

import fpinscala.state.RNG.Simple
import fpinscala.testing.Prop.{Falsified, Passed}
import org.scalatest.FunSuite

class PropTest extends FunSuite {
//  test("&&") {
//    val p1 = new Prop {
//      def check = true
//    }
//    val p2 = new Prop {
//      def check = false
//    }
//    assert((p1 && p2).check === false)
//  }

  test("Gen.choose") {
    val gen = Gen.choose(-2, 2)
    assert((0 until 10).map(i => gen.sample.run(Simple(i))._1) === Seq(-2, -1, -2, 0, -1, -2, 0, -1, -2, 0))
  }

  test("unit") {
    val gen = Gen.unit(1)
    assert((0 until 10).map(i => gen.sample.run(Simple(i))._1) === List.fill(10)(1))
  }

  test("Gen.boolean") {
    val gen = Gen.boolean
    assert((0 until 5).map(i => gen.sample.run(Simple(i))._1) === Seq(true, true, false, true, false))
  }

  test("static listOfN") {
    val gen = Gen.listOfN(5, Gen.choose(-2, 2))
    assert(gen.sample.run(Simple(0))._1 === List(-2, 0, -1, -1, 0))
  }

  test("listOfN") {
    val gen = Gen.choose(0, 5)
    val lists = for (seed <- 0 to 3) yield gen.listOfN(gen).sample.run(Simple(seed))

    assert(lists.map(_._1) === List(
      List(),
      List(),
      List(1),
      List(2, 1)
    ))
  }

  test("union") {
    val gen1 = Gen.choose(0, 5)
    val gen2 = Gen.choose(-5, 0)
    val united = Gen.listOfN(5, gen1.union(gen2)).sample.run(Simple(1))
    assert(united._1 === List(2, -4, 1, 3, -4))
  }

  test("weighted") {
    val gen1 = Gen.choose(0, 5)
    val gen2 = Gen.choose(-5, 0)
    val weightedGenList = Gen.listOfN(1000, gen1.weighted(.7)(gen2, .3)).sample.run(Simple(100))
    val valuesFromGen1 = weightedGenList._1.count(_ < 0)

    assert(valuesFromGen1 === 706)
  }

  test("run falsified") {
    val rng = Simple(2)
    val gen = Gen.choose(0, 5)
    val res = Prop.forAll(gen){ i =>
      i > 0
    }.run(100, rng)
    assert(res === Falsified("0", 2))
  }

  test("run passed") {
    val rng = Simple(2)
    val gen = Gen.choose(0, 5)
    val res = Prop.forAll(gen){ i =>
      i >= 0
    }.run(100, rng)
    assert(res === Passed)
  }

  test("&& passed") {
    val rng = Simple(0)
    val gen = Gen.choose(0, 5)
    val p1 = Prop.forAll(gen){ i =>
      i >= 0
    }
    val p2 = Prop.forAll(gen){ i =>
      i < 5
    }
    val res = (p1 && p2).run(100, rng)
    assert(res === Passed)
  }

  test("&& left failed") {
    val rng = Simple(2)
    val gen = Gen.choose(0, 5)
    val p1 = Prop.forAll(gen){ i =>
      i > 0
    }
    val p2 = Prop.forAll(gen){ i =>
      i < 5
    }
    val res = (p1 && p2).run(100, rng)
    assert(res === Falsified("0", 2))
  }

  test("&& right failed") {
    val rng = Simple(2)
    val gen = Gen.choose(0, 5)
    val p1 = Prop.forAll(gen){ i =>
      i >= 0
    }
    val p2 = Prop.forAll(gen){ i =>
      i > 4
    }
    val res = (p1 && p2).run(100, rng)
    assert(res === Falsified("1", 0))
  }
}
