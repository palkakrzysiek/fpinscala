package fpinscala.localeffects

import org.scalatest.{FunSuite, Matchers}

class STMapTest extends FunSuite with Matchers {

  val inMap: Map[String, Int] = Map("A" -> 1, "B" -> 2, "C" -> 3)

  test("STMap") {
    val result = ST.runST(new RunnableST[Map[String, Int]] {
      override def apply[S] = for {
        map1 <- STMap.empty[S, String, Int]
        map2 <- STMap.fromMap[S, String, Int](inMap)
        keys <- map2.keys
        _ <- keys.foldLeft(ST[S, Unit](()))((s, k) => for {
          _ <- s
          v <- map2.read(k)
          vWithDefault = v.getOrElse(0)
          _ <- map1 += (k, vWithDefault + 1)
        } yield ())
        frozen <- map1.freeze
      } yield frozen
    })
    result should be(Map("A" -> 2, "B" -> 3, "C" -> 4))
  }

}
