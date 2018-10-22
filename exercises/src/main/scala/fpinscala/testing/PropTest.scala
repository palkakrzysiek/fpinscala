package fpinscala.testing

import fpinscala.state.RNG.Simple
import org.scalatest.FunSuite

class PropTest extends FunSuite {
  test("&&") {
    val p1 = new Prop {
      def check = true
    }
    val p2 = new Prop {
      def check = false
    }
    assert((p1 && p2).check === false)
  }

  test("Gen.choose") {
    val gen = Gen.choose(-2, 2)
    assert((0 until 10).map(i => gen.sample.run(Simple(i))._1) === Seq(-2, -1, -2, 0, -1, -2, 0, -1, -2, 0))
  }
}
