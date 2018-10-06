package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def reverse: Stream[A] = {
    def loop(acc: Stream[A], rest: Stream[A]): Stream[A] = rest match {
      case Empty => acc
      case Cons(h, t) => loop(Cons(h, () => acc), t())
    }

    loop(empty, this)
  }

  def take(n: Int): Stream[A] = {
    def loop(acc: Stream[A], rest: Stream[A], nn: Int): Stream[A] = (rest,
      nn) match {
      case (Empty, _) => acc.reverse
      case (_, 0) => acc.reverse
      case (Cons(h, t), _) => loop(Cons(h, () => acc), t(), nn - 1)
    }

    loop(empty, this, n)
  }

  def drop(n: Int): Stream[A] = {
    def loop(rest: Stream[A], nn: Int): Stream[A] = if (nn <= 0) rest else rest
    match {
      case Empty => Empty
      case Cons(_, t) => loop(t(), nn - 1)
    }

    loop(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def loop(acc: Stream[A], rest: Stream[A]): Stream[A] = rest match {
      case Empty => acc.reverse
      case Cons(h, _) if !p(h()) => acc.reverse
      case Cons(h, t) => loop(Cons(h, () => acc), t())
    }

    loop(empty, this)
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = this
    .foldRight(empty: Stream[A]) { (e, acc) =>
      if (p(e)) cons(e, acc)
      else empty
    }

  def headOption: Option[A] = this.foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this
    .foldRight(empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = this
    .foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = this
    .foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this
    .foldRight(empty[B])((a, b) => f(a) append b)

  def startsWith[B](s: Stream[B]): Boolean = ???

  def toList: List[A] = this.foldRight(Nil: List[A])(_ +: _)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}