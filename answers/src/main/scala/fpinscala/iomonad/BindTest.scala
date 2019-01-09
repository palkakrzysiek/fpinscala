package fpinscala.iomonad

import language.higherKinds
import language.postfixOps

object BindTest extends App {

  def timeit(n: Int, tag: String)(task: => Unit): Unit = {
    val start = System.currentTimeMillis
    (0 to n).foreach { _ => task }
    val stop = System.currentTimeMillis
    println(tag + ": " + (stop - start) / 1000.0 + " seconds")
  }

  val N = 100000
  def go[F[_]](F: Monad[F])(unit: F[Unit])(f: F[Int] => Int): Unit = {
    import F.toMonadic
    f { (0 to N).map(i => F.map(unit)(_ => i)).foldLeft(F.unit(0)) {
      (f1,f2) => for {
        acc <- f1
        i <- f2
      } yield { // if (i == N) println("result: " + (acc+i))
                (acc + i)
              }
    }}
  }

  import fpinscala.parallelism.Nonblocking._

  object ParMonad extends Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A,B](pa: Par[A])(f: A => Par[B]) = Par.fork { Par.flatMap(pa)(f) }
  }

  def repeat[A](n: Int)(v: => A) = (0 to n).map(_ => v)

  for (nThreads <- 1 until 4) {
    println(s"== Using $nThreads threads:")
    val pool = java.util.concurrent.Executors.newFixedThreadPool(nThreads)
  repeat(5)(timeit(10, "Throw") { go(Throw)(Throw.unit(())) ( _ run ) })
    repeat(5)(timeit(10, "TailRec") { go(IO2b.TailRec)(IO2b.TailRec.unit(())) ( IO2b.run ) })
    repeat(5)(timeit(10, "Async") { go(IO2c.Async)(IO2c.Async.unit(()))(r => Par.run(pool) { IO2c.run(r) }) })
    repeat(5)(timeit(10, "ioMonad") { go[IO](ioMonad)(ioMonad.unit(()))(r => unsafePerformIO(r)(pool)) })
    repeat(5)(timeit(10, "Task#now") { go(Task)(Task.now(()))(r => r.run(pool)) })
    repeat(5)(timeit(10, "Task#forkUnit") { go(Task)(Task.forkUnit(()))(r => r.run(pool)) })
    repeat(5)(timeit(10, "ParMonad") { go(ParMonad)(ParMonad.unit(())) { p => Par.run(pool)(p) }})
    pool.shutdown()
  }



  /**
    * Throw: 0.795 seconds
    * TailRec: 0.861 seconds
    * Async: 0.654 seconds
    * ioMonad: 0.415 seconds
    * Task#now: 1.57 seconds
    * Task#forkUnit: 2.247 seconds
    * ParMonad: 1.461 seconds
    */

  // Par.run(pool)(ParMonad.forever { ParMonad.unit { println("woot") }})
}
