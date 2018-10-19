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

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r2) = rng.nextInt
    val (d, r3) = double(r2)
    ((i, d), r3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
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

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng1 => {
      ra(rng1) match {
        case (a, rng2) => rb(rng2) match {
          case (b, rng3) => (f(a, b), rng3)
        }
      }
    }
  }

  def intDoubleViaMap2: Rand[(Int, Double)] = map2(int, double)((i, d) => (i, d))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit[List[A]](Nil)) {
    (ra, rb) => map2(ra, rb)(_ +: _)
  }

  def intsViaSequence(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def flatMapWithoutHighlightingError[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (i, rng2) = f(rng)
    val x: RNG => (B, RNG) = g(i)
    val y: (B, RNG) = x(rng2)
    y
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => f(rng) match {
    case (a, rng2) => g(a)(rng2)
  }

//  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => s(rng) match {

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a =>
    unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]
  = flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  })


}

case class State[S, +A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a, b)


  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(run(_) match {
      case (a, nextS) => f(a).run(nextS)
    })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = sas
    .foldRight(unit[S, List[A]](Nil))((s, ss) => ss.map2(s)((_as, _a) => _a
      +: _as)).map(_.reverse)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
