package part1

import org.scalatest.funspec.AnyFunSpec
import part1.Chapter6._
import Utils._

class Chapter6Test extends AnyFunSpec {

  describe("Rand ops") {
    import Utils.DoubleOps
    implicit val precision: Double = 0.00001


    describe("SimpleRNG") {
      it("must generate rand number") {
        val rng = SimpleRNG(1)
        assert(rng.nextInt == (384748, SimpleRNG(25214903928L)))
      }
    }

    describe("ex 6.1 nonNegativeInt") {
      it("must handle negative int") {
        assert(nonNegativeInt(StubRng(-4)) == 3 -> StubRng(-3))
      }

      it("must emit positive int") {
        assert(nonNegativeInt(StubRng(5)) == 5 -> StubRng(6))
      }
    }

    describe("ex 6.2 double") {
      it("must emit double") {
        val (d, _) = double(StubRng(Int.MaxValue / 2))
        assert(d ~= 0.5)
      }
    }

    describe("ex 6.3") {
      describe("intDouble") {
        it("must generate two numbers") {
          val expectedInt = (Int.MaxValue * 0.9).toInt
          val expectedDouble = 0.9D
          val ((int, double), _) = intDouble(StubRng(expectedInt))

          assert(int == expectedInt && (double ~= expectedDouble))
        }
      }

      describe("doubleInt") {
        it("must generate two numbers") {
          val seed = (Int.MaxValue * 0.9).toInt
          val expectedDouble = 0.9D

          val ((double, int), _) = doubleInt(StubRng(seed))

          assert(int == seed + 1 && (double ~= expectedDouble))
        }
      }

      describe("double3") {
        it("must generate three numbers") {
          val seed = (Int.MaxValue * 0.9).toInt
          val step = (Int.MaxValue * 0.02).toInt

          val ((d1, d2, d3), _) = double3(StubRng(seed, step))

          assert((d1 ~= 0.9) && (d2 ~= 0.92) && (d3 ~= 0.94))
        }
      }
    }

    describe("ex 6.4") {
      it("ints must emit list of random Int values") {
        assert(ints(5)(StubRng(1))._1 == List(5, 4, 3, 2, 1))
      }
    }

    describe("ex 6.5") {
      it("must return double using map") {
        val (d, _) = double2(StubRng(Int.MaxValue))
        assert(d ~= 0.999999999)
      }
    }

    describe("ex 6.6") {
      it("must return double using map") {
        val (d, _) = double2(StubRng(500000000))
        assert(d ~= 0.23283064)
      }

      it("must return non negative even int") {
        val (i, _) = nonNegativeEven(StubRng(-55))
        assert(i == 54)
      }
    }

    describe("ex 6.7 map2") {
      it("must combine two generators") {
        val (res, _) = map2(nonNegativeEven, nonNegativeEven)(_ + _)(StubRng(50, 10))

        assert(res == 50 + 60)
      }

      it("ex 6.9 must combine two generators using map2ViaFlatmap") {
        val (res, _) = map2ViaFlatMap(nonNegativeEven, nonNegativeEven)(_ + _)(StubRng(50, 10))

        assert(res == 50 + 60)
      }

      it("ex 6.9 must map value using mapViaFlatMap") {
        val t = mapViaFlatMap(nonNegativeEven)(_ * 3)(StubRng(40))

        assert(t == (120, StubRng(41)))
      }

    }

    describe("ex 6.7 sequence") {
      it("must transform list of rand to rand of list") {
        val input: List[Rand[Int]] = List.fill(5)(nonNegativeEven)

        val (res: List[Int], _) = sequence(input)(StubRng(12, 2))
        assert(res == List(12, 14, 16, 18, 20))
      }
    }

    describe("ex 6.8 flatMap") {
      it("must pass forward RNG") {
        val rng = StubRng(4, 2)
        val (res: Seq[Int], _) = flatMap(nonNegativeEven)(positiveEven => ints(positiveEven))(rng)

        assert(res == List(12, 10, 8, 6))
      }
    }

    describe("nonNegativeLessThan") {
      it("must return without retry") {
        val t = nonNegativeLessThan(10)(StubRng(55))
        assert(t == (5, StubRng(56)))
      }

      it("must retry in case of large Int generated") {
        val t = nonNegativeLessThan(10)(StubRng(Int.MaxValue, -100))
        assert(t == (7, StubRng(Int.MaxValue - 200, -100)))
      }

    }

  }

  describe("State ops") {
    describe("ex 6.10") {
      describe("map") {
        it("must transform value without touching rng") {
          val result = State[Unit, Int](_ => (42, ())).map(_ * 2).run(())
          assert(result == (84, ()))
        }
      }

      describe("flatMap") {
        it("must pass forward state for next gen") {
          val intGen = State[RNG, Int](s => s.nextInt)
          assert(intGen.flatMap(_ => intGen).run(StubRng(4)) == (5, StubRng(6)))
        }
      }

      describe("unit") {
        it("must wrap value in state") {
          val value: State[Unit, Int] = State.unit(42)
          assert(value.run(()) == (42, ()))
        }
      }

      describe("sequence") {
        it("must convert List[State] to State[List, ...] passing forward state to next action in list") {
          val intGen = State[RNG, Int](s => s.nextInt)
          val list = List(intGen, intGen, intGen, intGen, intGen)

          assert(State.sequence(list).run(StubRng(1)) == (List(1,2,3,4,5), StubRng(6)))
        }
      }

      describe("map2") {
        it("must combine values from passed in States") {
          val intGen = State[RNG, Int](s => s.nextInt)
          assert(intGen.map2(intGen)(_ + _).run(StubRng(10)) == (10 + 11, StubRng(12)))
        }
      }

      describe("get") {
        it("must return state as value inside") {
          val value = State.unit[Int, Int](55)
          assert(State.get.run(55) == (55, 55))
        }
      }

      describe("set") {
        it("must set State's S to a given value") {
          assert(State.set("override").run("ignored value") == ((), "override"))
        }
      }

      describe("modify") {
        it("must modify given State's state") {
          assert(State.modify((_: Int) * 2).run(10) == ((), 20))
        }
      }
    }
  }

  describe("Machine Simulation") {
    import Chapter6.MachineSimulation._
    it("must consume given inputs and return valid Machine state") {
      val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn)
      val m = Machine(locked = true, candies = 10, coins = 2)

      assert(simulateMachine(inputs).run(m)._1 == (5, 7))
    }

    it("must ignore inputs because machine is empty") {
      val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn)
      val m = Machine(locked = true, candies = 0, coins = 2)

      assert(simulateMachine(inputs).run(m)._1 == (2, 0))
    }


  }
}
