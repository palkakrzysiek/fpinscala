import scala.List
import scala.Nil

List(1, 2, 3).foldRight(Nil: List[Int])((e, s) => e :: s)

1 :: (2 :: (3 :: Nil))


List(1, 2, 3).foldLeft(Nil: List[Int])((s, e) => e :: s)

3 :: (2 :: (1 :: Nil))

