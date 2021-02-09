package part2

import org.scalatest.funspec.AnyFunSpec

import java.util.concurrent.{Executors}

class Chapter7Tests extends AnyFunSpec {

  def sleepAndReturn[A](a: A, timeoutInMillis: Long = 100L): A = {
    println(s"${Thread.currentThread().getName}")
    Thread.sleep(timeoutInMillis)
    a
  }

  def printAndF[A, B](f: A => B): A => B = a => {
    println(s"${Thread.currentThread().getName}")
    f(a)
  }

  describe("Blocking Par ops") {
    val es = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())

    import Chapter7.Blocking.Par._
    import Chapter7.Blocking.Par.{run => eval}

    def unwrap[A](par: Par[A]): A =
      eval(es)(par).get

    describe("unit") {
      it("should just wrap a value") {
        assert(unwrap(unit(42)) == 42)
      }
    }

    describe("map2") {
      it("should combine 2 computations asynchronously and combine results using provided function ") {

        val pa = lazyUnit(sleepAndReturn(42))
        val pb = lazyUnit(sleepAndReturn(99, 200L))

        assert(unwrap(map2(pa, pb)(_ + _)) == 42 + 99)
      }
    }

    describe("map") {
      it("should map value using provided function") {
        val pa = lazyUnit(sleepAndReturn(50))
        assert(unwrap(map(pa)(_ + 100)) == 150)
      }
    }

    describe("delay") {
      it("should delay submission of task to pool") {
        assert(unwrap(delay(unit(sleepAndReturn(50)))) == 50)
      }
    }

    describe("asyncF") {
      it("must lift A => B to A => Par[B]") {
        val f: Int => Par[Int] = asyncF((_: Int) + 100)

        assert(unwrap(f(44)) == 144)
      }
    }

    describe("sequence") {
      it("must convert Seq[Par[_]] to Par[Seq[_]]") {
        val value: List[Par[Int]] = List.fill(5)(lazyUnit(sleepAndReturn(100)))
        val res: List[Int] = unwrap(sequence(value))

        assert(res == List(100,100,100,100,100))
      }
    }

    describe("parMap") {
      it("must fork and then asynchronously map each value in list using lifted function") {
        val value: Par[List[Int]] = parMap(List(1,2,3,4,5))(printAndF(_ + 10))

        assert(unwrap(value) == List(11,12,13,14,15))
      }
    }

    describe("parFilter") {
      it("must fork and then asynchronously match") {
        val value: Par[List[Int]] = parFilter(List(1,2,3,4,5))(printAndF(_ < 3))

        assert(unwrap(value) == List(1,2))
      }
    }

    describe("join") {
      it("must flatten Par[Par[A]] to Par[A]") {
        val value = lazyUnit(lazyUnit(sleepAndReturn(42)))

        assert(unwrap(join(value)) == 42)
      }
    }

    describe("flatMap") {
      it("must chain computations") {
        val x = lazyUnit(sleepAndReturn(5))

        val value = flatMap(x)(a => {
          println("processing after a")
          lazyUnit(sleepAndReturn(10))
        })

        assert(unwrap(value) == 10)
      }
    }



  }

  describe("Non Blocking Par ops") {
    val es = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())

    import Chapter7.NonBlocking._
    import Chapter7.NonBlocking.{run => doRun}

    def unwrap[A](par: Par[A]): A =
      doRun(es)(par)

    describe("unit") {
      it("should just wrap a value") {
        assert(unwrap(unit(42)) == 42)
      }
    }

    describe("map2") {
      it("should combine 2 computations asynchronously and combine results using provided function ") {

        val pa = lazyUnit(sleepAndReturn(42))
        val pb = lazyUnit(sleepAndReturn(99, 200L))

        assert(unwrap(map2(pa, pb)(_ + _)) == 42 + 99)
      }
    }

    describe("map") {
      it("should map value using provided function") {
        val pa = lazyUnit(sleepAndReturn(50))
        assert(unwrap(map(pa)(_ + 100)) == 150)
      }
    }

    describe("delay") {
      it("should delay submission of task to pool") {
        assert(unwrap(delay(unit(sleepAndReturn(50)))) == 50)
      }
    }

    describe("asyncF") {
      it("must lift A => B to A => Par[B]") {
        val f: Int => Par[Int] = asyncF((_: Int) + 100)

        assert(unwrap(f(44)) == 144)
      }
    }

    describe("sequence") {
      it("must convert Seq[Par[_]] to Par[Seq[_]]") {
        val value: List[Par[Int]] = List.fill(5)(lazyUnit(sleepAndReturn(100)))
        val res: List[Int] = unwrap(sequence(value))

        assert(res == List(100,100,100,100,100))
      }
    }

    describe("parMap") {
      it("must fork and then asynchronously map each value in list using lifted function") {
        val value: Par[List[Int]] = parMap(List(1,2,3,4,5))(printAndF(_ + 10))

        assert(unwrap(value) == List(11,12,13,14,15))
      }
    }

    describe("parFilter") {
      it("must fork and then asynchronously match") {
        val value: Par[List[Int]] = parFilter(List(1,2,3,4,5))(printAndF(_ < 3))

        assert(unwrap(value) == List(1,2))
      }
    }

    describe("join") {
      it("must flatten Par[Par[A]] to Par[A]") {
        val value = lazyUnit(lazyUnit(sleepAndReturn(42)))

        assert(unwrap(join(value)) == 42)
      }
    }

    describe("flatMap") {
      it("must chain computations") {
        val x = lazyUnit(sleepAndReturn(5))

        val value = flatMap(x)(a => {
          println("processing after a")
          lazyUnit(sleepAndReturn(10))
        })

        assert(unwrap(value) == 10)
      }
    }



  }

}
