package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case _ => throw new NoSuchElementException()
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Cons(_, t) => drop(t, n - 1)
      case _ => throw new IndexOutOfBoundsException()
    }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case list => list
  }

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def loop(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => acc
      case Cons(h, t) => loop(t, append(acc, Cons(h, Nil)))
    }

    loop(l, Nil)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, counter) => counter + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =  l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def append2[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((l1h, acc) => Cons(l1h, acc))

  def flatten[A](ls: List[List[A]]): List[A] = foldRight(ls, Nil: List[A])(append)

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def d2s(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A]) {
    (h, t) => if (f(h)) Cons(h, t) else t
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((as, a) => Cons(a, as))

  def addElements(as: List[Int], bs: List[Int]): List[Int] = {
    def loop(as: List[Int], bs: List[Int], acc: List[Int]): List[Int] = (as, bs) match {
      case (Cons(ah, at), Cons(bh, bt)) => loop(at, bt, Cons(ah + bh, acc))
      case _ => reverse(acc)
    }
    loop(as, bs, Nil)
  }

  def zipWith[A, B, R](as: List[A], bs: List[B])(f: (A, B) => R): List[R] = {
    def loop(as: List[A], bs: List[B], acc: List[R]): List[R] = (as, bs) match {
      case (Cons(ah, at), Cons(bh, bt)) => loop(at, bt, Cons(f(ah, bh), acc))
      case _ => reverse(acc)
    }
    loop(as, bs, Nil)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    /**
      *
      * @param rSup remaining sup
      * @param rSub remaining sub
      * @param rmSup remaining matching sup
      * @param rmSub remaining matching sub
      * @return sup contains sub
      */
    @tailrec
    def loop(rSup: List[A], rSub: List[A], rmSup: List[A], rmSub: List[A]): Boolean = (rSup, rSub, rmSup, rmSub) match {
      case (_, _, _, Nil) => true
      case (Nil, _, _, _) => false
      case (_, _, Cons(irmSuph, irmSupt), Cons(irmSubh, irmSubt)) if irmSuph == irmSubh => loop(rSup, rSub, irmSupt, irmSubt)
      case (Cons(_, irSupt), _, _, _) => loop(irSupt, rSub, irSupt, sub)
    }

    loop(sup, sub, sup, sub)
  }


}
