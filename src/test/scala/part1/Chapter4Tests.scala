package part1

import org.scalatest.funspec.AnyFunSpec

class Chapter4Tests extends AnyFunSpec {

  import Chapter4._

  describe("Option ops ex 4.1") {

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

}
