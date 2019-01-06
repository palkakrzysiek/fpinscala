package fpinscala
package applicative

import monads.Functor
import state._
import StateUtil._
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
  def product[G[_]](G: Applicative[G]): Applicative[Lambda[x => (F[x], G[x])]] = {
    val self = this
    new Applicative[Lambda[x => (F[x], G[x])]] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

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

    // without overriding this map, applicative's map definition implemented in terms
    // of apply thus flatMap causes stack overflow
    override def map[A, B](st: State[S, A])(f: A => B): State[S, B] = st map f

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
  Monad[Lambda[x => F[N[x]]]] = {
    new Monad[Lambda[x => F[N[x]]]] {
      override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))

      override def flatMap[A, B](fna: F[N[A]])(f: A => F[N[B]]): F[N[B]] = {
        def traversed(na: N[A]): F[N[N[B]]] = T.traverse(na)(f)
        F.flatMap(fna)((na: N[A]) => F.map(traversed(na))(N.join))
      }
    }
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  type Id[A] = A
  implicit val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  implicit val streamApplicative: Applicative[Stream] = new Applicative[Stream] {

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

  import Applicative._
  import Monad._

//  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse(fa)(a => Stream.continually(f(a)))(streamApplicative).head

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)


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

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a: A, b: B) => ((), f(b, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
    (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    implicit val GH: Applicative[Lambda[x => (G[x], H[x])]] = G.product(H)
    traverse[Lambda[x => (G[x], H[x])], A, B](fa)(x => (f(x), g(x)))
  }


  def compose[G[_]](implicit G: Traverse[G]): Traverse[Lambda[x => F[G[x]]]] = {
    val self = this
    new Traverse[Lambda[x => F[G[x]]]] {
      override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }
  }

}

object Traverse {
  implicit val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldRight(implicitly[Applicative[G]].unit(List.empty[B])) {
        case(a, gAcc) => implicitly[Applicative[G]].map2(f(a), gAcc)((a, acc) => a +: acc)
      }
  }

  lazy val optionTraverse = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = fa
      .fold(implicitly[Applicative[G]].unit(None): G[Option[B]])(a =>
        implicitly[Applicative[G]].map(f(a))(b => Some(b)))
  }

  lazy val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] =
      implicitly[Applicative[G]].map2(f(fa.head),
        listTraverse.traverse(fa.tail)((ta: Tree[A]) => traverse(ta)(f)))(
        (b: B, tbs: List[Tree[B]]) => Tree(b, tbs)
      )
  }

  implicit class TreeTraversableOpts[A](in: Tree[A]) {
    def toList: List[A] = treeTraverse.toList(in)
    def reverse: Tree[A] = treeTraverse.reverse(in)
  }
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
