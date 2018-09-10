package fpinscala.gettingstarted

import org.scalatest._

class PolymorphicFunctionsTest extends FlatSpec with Matchers {
  import PolymorphicFunctions._
  val comp = (a: Int, b: Int) => a > b
  for (sortedArray <- Seq(Array(), Array(1), Array(1,2), Array(1,1), Array(1,2,3))) {
    "A comparison checker" should s"return true for the array of $sortedArray" in {
      isSorted(sortedArray, comp) should be (true)
    }
  }
  "A comparison checker" should "return false" in {
    val sorted1 = Array(1,4,3)
    isSorted(sorted1, comp) should be (false)
  }

}
