package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A], d: Int = 0): Int = t match {
    case Leaf(_) => d + 1
    case Branch(l, r) => depth(l, d + 1) max depth(r, d + 1)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeUsingFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _)

  def maximumUsingFold(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depthUsingFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((a, b) => 1 + (a max b))

  def mapUsingFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(l => Leaf(f(l)): Tree[B])((l, r) => Branch(l, r))

}