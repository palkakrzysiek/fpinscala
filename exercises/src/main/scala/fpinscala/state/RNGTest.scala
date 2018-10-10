package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.{Matchers, WordSpec}

class RNGTest extends WordSpec with Matchers {

  val rng = Simple(1L)

  "RNG.ints" in {
    RNG.ints(3)(rng) should be ((List(-549383847, -1151252339, 384748),Simple
    (245470556921330L)))
  }

  "RNG.doubleViaMap" in {
    RNG.doubleViaMap(rng) should be (1.79162249052507E-4,Simple(25214903928L))
  }

}
