package part1

import org.scalatest.funspec.AnyFunSpec

class Chapter4Tests extends AnyFunSpec {

  import Chapter4._

  describe("Option ops") {

    describe("ex 4.1") {
      describe("map") {
        it("should transform value using function f") {
          assert(Option(42).map(_ * 2) == Option(84))
        }
      }

      describe("getOrElse") {
        it("should return provided value in case of None") {
          assert(Option.empty[Int].getOrElse(55) == 55)
        }
      }

      describe("orElse") {
        it("should return provided option if none") {
          assert(Option.empty[Int].orElse(Option(55)) == Option(55))
        }

        it("should return itself if it's Some") {
          assert(Option(42).orElse(Option(55)) == Option(42))
        }
      }

      describe("filter") {
        it("should return none if predicate returned false") {
          assert(Option(100).filter(_ > 200) == None)
        }

        it("filter should return itself if predicate returned false") {
          assert(Option(100).filter(_ % 2 == 0) == Option(100))
        }
      }

      describe("flatMap") {
        it("should apply given function to value") {
          assert(Option(42).flatMap(a => Some(a * 2)) == Some(84))
        }
      }
    }

    describe("ex 4.2 variance") {
      it("should match expected value") {
        assert(Option.variance(Seq(600, 470, 170, 430, 300)).getOrElse(-1) == 21704)
      }
    }

    describe("ex 4.3 map2") {
      it("should lift Function2 to consume two option values") {
        val f: (Int, Int) => Int = (_: Int) * (_: Int)
        assert(Option.map2(Option(42), Option(2))(f) == Option(84))
      }
    }

    describe("ex 4.4 sequence") {
      val list = List(Option(2), Option(4))
      it("must return Some(list) if all elements was Some") {
        assert(Option.sequence(list) == Some(List(2, 4)))
      }

      it("must return None if any element in the list was None") {
        assert(Option.sequence(List(Option(42), None, Option(22))) == None)
      }

      it("must match sequence2") {
        assert(Option.sequence(list)  == Option.sequence2(list))
      }
    }

    describe("ex 4.5 traverse") {
      it("it must apply a f to each value in the list before sequencing them to Option[List]") {
        val list = List("1", "3", "5")

        assert(Option.traverse(list)(x => Option.Try(x.toInt)) == Some(List(1, 3, 5)))
      }
    }

    describe("misc") {
      it("Option.apply must return None in case of null") {
        assert(Option[String](null) == None)
      }

      it("lift must make 'f :: a b' 'f :: Option a b'") {
        assert(Option.lift((_: Int) + 3)(Option(4)) == Option(7))
      }

      it("try must return None if function failed") {
        assert(Option.Try(4 / 0) == None)
      }

      it("mean returns None if seq was empty") {
        assert(Option.mean(Seq()) == None)
      }
    }
  }

  describe("Either ops") {

    describe("ex 4.7") {

      it("map should transform value inside either in case of Right") {
        assert(Right(10).map(_ * 20) == Right(200))
      }

      it("map should not transform value inside either in case of Left") {
        assert((Left("Error?"): Either[String, Int]).map(_ * 20) == Left("Error?"))
      }

      it("flatMap should apply f to value inside either in case of Right") {
        assert(Right(10).flatMap(a => Right(a * 20)) == Right(200))
      }

      it("flatMap should apply f to value inside either in case of Left") {
        assert((Left("Error?"): Either[String, Int]).flatMap(a => Right(a * 20)) == Left("Error?"))
      }

      it("orElse should return provided value on case of Left") {
        assert(Left("Error?").orElse(Right(2)) == Right(2))
      }

      it("orElse should return Right's value on case of Right") {
        assert(Right(4).orElse(Right(2)) == Right(4))
      }

      it("should lift Function2 to consume two either values") {
        val f: (Int, Int) => Int = _ + _
        assert(Right(42).map2(Right(2))(f) == Right(44))
      }

      it("sequence must transform List[Either] to Either[E, List]") {
        val list = List(Right(42), Right(22))
        assert(Either.sequence(list) == Right(List(42, 22)))
      }

      it("traverse must transform List[A] using A => Either to Either[E, List]") {
        val list = List("1","2","3","4","5")
        assert(Either.traverse(list)(a => Either.Try(a.toInt)) == Right(List(1,2,3,4,5)))
      }

      describe("ex 4.8") {
        describe("It's not possible because we need to combine `left` values, " +
          "that's possible after changes to map, sequence... ") {
        }
      }
    }

    describe("misc") {

      it("mean should return Left in case of empty List") {
        assert(Either.mean(Seq()) == Left("mean of empty list!"))
      }

      it("mean should return Right in case of empty List") {
        assert(Either.mean(Seq(5, 15)) == Right(10.0))
      }

      it("try over division by 0 should return exception in Left") {
        assert(Either.Try(2 / 0).isInstanceOf[Left[_]])
      }

      it("try should return value in Right") {
        assert(Either.Try(2 / 1) == Right(2))
      }


    }
  }

}
