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

  for ((l, m) <- Seq(
    (Leaf(0), 0),
    (Branch(Leaf(10), Leaf(0)), 10),
    (Branch(Leaf(0), Leaf(10)), 10),
    (Branch(Branch(Leaf(0), Leaf(0)), Leaf(10)), 10)
  )) {
    "Tree.maximum" should s"return $m for $l" in {
      Tree.maximum(l) should be (m)
    }
  }

  for ((l, d) <- Seq(
    (Leaf(0), 1),
    (Branch(Leaf(10), Leaf(0)), 2),
    (Branch(Branch(Leaf(0), Leaf(0)), Leaf(10)), 3),
    (Branch(Leaf(10), Branch(Leaf(0), Leaf(0))), 3)
  )) {
    "Tree.depth" should s"return $d for $l" in {
      Tree.depth(l) should be (d)
    }
  }

  for ((l, m) <- Seq(
    (Leaf(0), Leaf(1)),
    (Branch(Leaf(10), Leaf(0)), Branch(Leaf(11), Leaf(1))),
    (Branch(Branch(Leaf(0), Leaf(0)), Leaf(10)), Branch(Branch(Leaf(1), Leaf(1)), Leaf(11))),
    (Branch(Leaf(10), Branch(Leaf(0), Leaf(0))), Branch(Leaf(11), Branch(Leaf(1), Leaf(1))))
  )) {
    "Tree.map" should s"return $m for $l by applying x => x + 1 on each element fot the tree" in {
      Tree.map(l)(_ + 1) should be (m)
    }
  }

  for ((l, s) <- Seq(
    (Leaf(""), 1),
    (Branch(Leaf(""), Leaf("")), 2),
    (Branch(Branch(Leaf(""), Leaf("")), Leaf("")), 3),
    (Branch(Leaf(""), Branch(Leaf(""), Leaf(""))), 3)
  )) {
    "Tree.sizeUsingFold" should s"return $s for $l" in {
      Tree.sizeUsingFold(l) should be (s)
    }
  }

  for ((l, m) <- Seq(
    (Leaf(0), 0),
    (Branch(Leaf(10), Leaf(0)), 10),
    (Branch(Leaf(0), Leaf(10)), 10),
    (Branch(Branch(Leaf(0), Leaf(0)), Leaf(10)), 10)
  )) {
    "Tree.maximumUsingFold" should s"return $m for $l" in {
      Tree.maximumUsingFold(l) should be (m)
    }
  }

  for ((l, d) <- Seq(
    (Leaf(0), 1),
    (Branch(Leaf(10), Leaf(0)), 2),
    (Branch(Branch(Leaf(0), Leaf(0)), Leaf(10)), 3),
    (Branch(Leaf(10), Branch(Leaf(0), Leaf(0))), 3)
  )) {
    "Tree.depthUsingFold" should s"return $d for $l" in {
      Tree.depthUsingFold(l) should be (d)
    }
  }

  for ((l, m) <- Seq(
    (Leaf(0), Leaf(1)),
    (Branch(Leaf(10), Leaf(0)), Branch(Leaf(11), Leaf(1))),
    (Branch(Branch(Leaf(0), Leaf(0)), Leaf(10)), Branch(Branch(Leaf(1), Leaf(1)), Leaf(11))),
    (Branch(Leaf(10), Branch(Leaf(0), Leaf(0))), Branch(Leaf(11), Branch(Leaf(1), Leaf(1))))
  )) {
    "Tree.mapUsingFold" should s"return $m for $l by applying x => x + 1 on each element fot the tree" in {
      Tree.mapUsingFold(l)(_ + 1) should be (m)
    }
  }

}
