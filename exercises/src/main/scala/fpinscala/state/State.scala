package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, r) => (Math.abs(i), r)
  }

  def double(rng: RNG): (Double, RNG) = rng.nextInt match {
    case (i, r) => (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r2) = rng.nextInt
    val (d, r3) = double(r2)
    ((i, d), r3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(acc: List[Int], n: Int, rng: RNG): (List[Int], RNG) = {
      if (n <= 0) (acc, rng)
      else rng.nextInt match {
        case (v, rng2) => go(v +: acc, n - 1, rng2)
      }
    }
    go(Nil, count, rng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => s(rng) match {
    case (a, nextRNG) => (f(a), nextRNG)
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

//  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
//    val m1: Rand[B => C] = map(ra)(f.curried)
//  }

  def intDoubleViaMap2: Rand[(Int, Double)] = map2(int, double)((i, d) => (i, d))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
