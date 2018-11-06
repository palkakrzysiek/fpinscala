package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, FunSuite, Matchers}

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  import org.scalatest.prop.PropertyChecks

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a2 andThen a1

    override def zero: A => A = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  //  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  //  import fpinscala.testing._
  //  import Prop._
  //  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.fold(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).fold(m.zero)(m.op)


  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => f(a, _))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(b => f(_, b))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val s = as.size
    if (s == 0) m.zero
    else if (s == 1) f(as(0))
    else m.op(foldMapV(as.take(s/2), m)(f), foldMapV(as.drop(s/2), m)(f))
  }

  val isOrderedMonoid = new Monoid[Int => Int => Boolean] {

    override def zero: Int => Int => Boolean = a => b => a <= b

    override def op(a1: Int => Int => Boolean, a2: Int => Int => Boolean): Int => Int => Boolean = ???
  }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    ???

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    ???

  lazy val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    ???

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    ???

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
    ???
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    ???

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    ???

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    ???

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    ???

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    ???

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    ???
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    ???

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    ???
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    ???

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    ???
}

class MonoidTest extends FunSuite with PropertyChecks with Matchers{

  import Monoid._

  def monoidLaws[A](m: Monoid[A])(implicit gen: Arbitrary[A]) = {
    forAll {
      a: A => m.op(m.zero, a) should be(a)
    }
    forAll {
      (a1: A, a2: A, a3: A) =>
        m.op(m.op(a1, a2), a3) should be(m.op(a1, m.op(a2, a3)))
    }
  }

  test("monoidLaws") {
    monoidLaws(stringMonoid)
    monoidLaws(intAddition)
    monoidLaws(intMultiplication)
    monoidLaws(booleanAnd)
    monoidLaws(booleanOr)
  }

  test("foldRight") {
    foldRight(List("a", "b", "c"))("[start]")((a: String, acc: String) => acc + a) should be ("[start]cba")
  }

  test("foldLeft") {
    foldLeft(List("a", "b", "c"))("[start]")((acc: String, a: String) => acc + a) should be ("[start]abc")
//    List("a", "b", "c").foldLeft("[start]")((acc: String, a: String) => acc + a) should be ("[start]abc")
  }

  test("foldMapV") {
    val m = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2

      override def zero: Int = 0
    }
    foldMapV(Vector(), m)(identity) should be (0)
    foldMapV(Vector(1), m)(identity) should be (1)
    foldMapV(Vector(1, 2), m)(identity) should be (3)
    foldMapV(Vector(1, 2, 3), m)(identity) should be (6)
  }

}