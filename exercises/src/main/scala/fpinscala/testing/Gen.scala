package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


case class Prop(run: (TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (n, rng) => run(n, rng) match {
      case Passed => p.run(n, rng)
      case failed => failed
    }
  }
  def ||(p: Prop): Prop = Prop {
    (n, rng) => run(n, rng) match {
      case Falsified(_, _) => p.run(n, rng)
      case passed => passed
    }
  }
}


object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }


  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = flatMap(a => Gen(State.unit(f(a))))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => Gen(State(RNG.sequence(List.fill(s)(sample.run)))))
  def union(other: Gen[A]): Gen[A] = boolean.flatMap(if(_) this else other)
  def weighted(thisWeight: Double)(other: Gen[A], otherWeight: Double): Gen[A] = {
    def isThis(rand: Double) = rand > thisWeight
    double.map(isThis).flatMap(if(_) this else other)
  }
  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegativeLessThan(stopExclusive-start - 1)).map(start + _))
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_%2 == 0))
  def double: Gen[Double] = Gen(State(RNG.double))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State(RNG.sequence(List.fill(n)(g.sample.run))))
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(i => listOfN(i, g))
}

case class SGen[+A](forSize: Int => Gen[A])

