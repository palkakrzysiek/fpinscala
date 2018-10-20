package fpinscala.parallelism

import java.util.concurrent.{Executors, TimeUnit}

import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.TimeoutException

class ParTest extends FunSuite with Matchers {

  import Par._

  private def withExecutionTimeInNanos[A](block: => A): (A, Long) = {
    val startTime = System.nanoTime()
    val res = block
    val endTime = System.nanoTime()
    (res, endTime - startTime)
  }

  private def miliToNano(x: Int): Long = x * 1000000L

  test("map2 parallel") {
    val executor = Executors.newFixedThreadPool(2)

    val p200: Par[Int] = lazyUnit {
      Thread.sleep(200)
      200
    }
    val p400: Par[Int] = lazyUnit {
      Thread.sleep(400)
      400
    }

    withExecutionTimeInNanos(map2(p200, p400)(_ + _)(executor).get()) match {
      case (res, time: Long) =>
        res should be(600)
        time should (be > miliToNano(400) and be < miliToNano(600))
    }
    executor.shutdown()
  }

  test("map2 timeout") {
    val executor = Executors.newFixedThreadPool(2)

    val p200: Par[Int] = lazyUnit {
      Thread.sleep(200)
      200
    }
    val p400: Par[Int] = lazyUnit {
      Thread.sleep(400)
      400
    }

    map2(p200, p400)(_ + _)(executor).get(600, TimeUnit.MILLISECONDS) should be(600)

    assertThrows[TimeoutException] {
      map2(p200, p400)(_ + _)(executor).get(399, TimeUnit.MILLISECONDS)
    }
    executor.shutdown()
  }

  test("asyncF") {
    val executor = Executors.newFixedThreadPool(2)
    asyncF((i: Int) => i + 1)(1)(executor).get() should be(2)
  }

  test("parMap") {
    val parallelism = 50
    val executor = Executors.newFixedThreadPool(parallelism)
    val slowIncrement: Int => Int = { i => {
      Thread.sleep(300)
      i + 1
    }
    }
    val task = parMap((1 to parallelism).toList)(slowIncrement)
    val overheadBuffer = 500
    withExecutionTimeInNanos {
      task(executor).get()
    } match {
      case (res, time) =>
        res should be((2 to parallelism + 1).toList)
        time should (be >= miliToNano(300) and be < miliToNano(300 + overheadBuffer))
    }
  }

  test("parFilter") {
    val parallelism = 500
    val executor = Executors.newFixedThreadPool(parallelism)

    def filter: Int => Boolean = _ % 2 == 0
    def slowFilter: Int => Boolean = {
      Thread.sleep(300)
      filter
    }
    lazy val task = parFilter((1 to parallelism).toList)(slowFilter)
    val overheadBuffer = 500
    withExecutionTimeInNanos {
      task(executor).get()
    } match {
      case (res, time) =>
        res should be((1 to parallelism).filter(filter).toList)
        time should (be >= miliToNano(300) and be < miliToNano(300 + overheadBuffer))
    }
  }
}


