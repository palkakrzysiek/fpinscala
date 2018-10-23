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

trait Prop { self =>
  def check: Boolean
  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = self.check && p.check
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG, A]) {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???

}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegativeLessThan(stopExclusive-start - 1)).map(start + _))
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_%2 == 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State(RNG.sequence(List.fill(n)(g.sample.run))))
}

trait SGen[+A] {

}

