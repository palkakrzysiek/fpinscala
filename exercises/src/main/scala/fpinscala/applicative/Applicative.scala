package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._
import com.sun.net.httpserver.Authenticator
import com.sun.net.httpserver.Authenticator.Failure
import monoids._

import language.{higherKinds, implicitConversions, reflectiveCalls}

trait Applicative[F[_]] extends Functor[F] {

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(fa))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((ab, a) => ab(a))

  def unit[A](a: => A): F[A]

  def sequence[A](fas: List[F[A]]): F[List[A]] = fas.foldRight(unit(List.empty[A]))((fa: F[A], acc: F[List[A]]) => map2(fa, acc)(_ :: _))

  def sequenceMap[K,V](ofa: Map[K, F[V]]): F[Map[K, V]] = ofa.foldLeft(unit(Map.empty[K, V])) {
    case (fAcc, (k, fv)) => map2(fv, fAcc)((v, acc) => acc + (k -> v))
  }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???

  //  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A) = (self.unit(a), G.unit(a))

      override def map2[A, B, C](faga: (F[A], G[A]), fbgb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = (faga, fbgb) match {
        case ((fa, ga), (fb, gb)) => (self.map2(fa, fb)(f), G.map2(ga, gb)(f))
      }


    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[Lambda[x => F[G[x]]]] = {
    val self = this
    new Applicative[Lambda[x => F[G[x]]]] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))

    }
  }

}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def nonKleisliCompose[G[_]](G: Monad[G]): Monad[Lambda[x => F[G[x]]]] = {
    val self = this
    new Monad[Lambda[x => F[G[x]]]] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      def swapArrow[A,M[_],N[_],B](fafgb: A=>M[N[B]]): A=>N[M[B]] = fafgb.andThen(swap)

      // distributive law
      def swap[A, M[_], N[_]](mna: M[N[A]]): N[M[A]] = ???

      override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] = self.flatMap(ma)((ga: G[A]) => swap(G.flatMap(ga)((a: A) => swapArrow(f)(a))))
    }
  }

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[Either[E, ?]] = new Monad[Either[E, ?]] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[State[S, ?]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
  Monad[Lambda[x => F[N[x]]]] = ???

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
      f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  //  def validationApplicative[E]: Applicative[({type λ[α] = Validation[E,α]})#λ] = new Applicative[({type λ[α] = Validation[E,α]})#λ] {
  //    override def unit[A](a: => A): Validation[E, A] = Success(a)
  //    override def map2[A, B, C](fa:  Validation[E, A], fb:  Validation[E, B])(f:  (A, B) => C): Validation[E, C] = (fa, fb) match {
  //      case (Success(a), Success(b)) => Success(f(a, b))
  //      case (f @ Failure(_, _), Success(_)) => f
  //      case (Success(_), f @ Failure(_, _)) => f
  //      case (Failure(hl, tl), Failure(hr, tr)) => Failure(hl, tl ++ Vector(hr) ++ tr)
  //    }

  def validationApplicative[E]: Applicative[Validation[E, ?]] = new Applicative[Validation[E, ?]] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (f@Failure(_, _), Success(_)) => f
      case (Success(_), f@Failure(_, _)) => f
      case (Failure(hl, tl), Failure(hr, tr)) => Failure(hl, tl ++ Vector(hr) ++ tr)
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[Applicative.Const[M, ?]] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A, B](fa: F[A])(f: A => B): F[B] = ???

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[Applicative.Const[B, ?], A, Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[State[S, ?], A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
    (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???

}

object Traverse {
  val listTraverse = ???

  val optionTraverse = ???

  val treeTraverse = ???
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
