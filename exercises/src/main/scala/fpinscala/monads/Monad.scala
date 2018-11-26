package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._

import language.{higherKinds, reflectiveCalls}


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  // partially-applied type named "IntOrA"
  type IntOrA[A] = Either[Int, A]

  // type projection implementing the same type anonymously (without a name).
  // val x: ({type L[A] = Either[Int, A]})#L = Left(2)

  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = lma.foldRight(unit(Nil: List[A]))((ma: M[A], mAcc: M[List[A]]) =>
    flatMap(mAcc)(acc => map(ma)(a => a :: acc))
  )

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = sequence(la.map(f))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(b => g(b))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    ms.foldRight(unit(Nil: List[A]))((a: A, mAcc: M[List[A]]) =>
      flatMap(f(a))(if(_) map(mAcc)(a :: _) else mAcc )
    )
  }

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = compose((_: Unit) => ma, f)()

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.apply(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)
  }

  lazy val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

/*
EX 11.10

# Left identity
compose(f, unit) == f // <1>
<==>
flatMap(x)(unit) == x

Because of
  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
  a => flatMap(f(a))(g)

LHS of <1> (right identity) can be rewritten

compose(f, unit)(v) == flatMap(f)(unit)(v)

then <1> is
flatMap(f)(unit)(v) == f(v) // replacing f with x
flatMap(x)(unit)(v) == x(v) // Q.E.D.

# Right identity

compose(unit, f)(v) == f(v) // <2>
<==>
flatMap(unit(y))(f) == f(y) <3>

Because of
  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
  a => flatMap(f(a))(g)

LHS of <2> can be rewritten
 compose(unit, f)(v) ==
 (a => flatMap(unit(a))(f))(v) ==
 flatMap(unit(v))(f)), putting this into <2>

flatMap(unit(v))(f)) == f(v)
<==>
flatMap(unit(y))(f)) == f(y) // replacing v with y

Q.E.D

 */

/*
11.11
Right identity for Option
flatMap(x)(unit) == x

flatMap(None)(unit) =
  flatMap(None)(Some(_)) = // using unit of Option
  x // applying Some(_) to flatMap of Option
jeez, enough
*/

