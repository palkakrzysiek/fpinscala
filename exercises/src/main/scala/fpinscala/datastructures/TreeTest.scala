package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers {

  for ((l, s) <- Seq(
    (Leaf(""), 1),
    (Branch(Leaf(""), Leaf("")), 2),
    (Branch(Branch(Leaf(""), Leaf("")), Leaf("")), 3),
    (Branch(Leaf(""), Branch(Leaf(""), Leaf(""))), 3)
  )) {
    "Tree.size" should s"return $s for $l" in {
      Tree.size(l) should be (s)
    }
  }

}
