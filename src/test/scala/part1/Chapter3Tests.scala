package part1

import org.scalatest.funspec.AnyFunSpec


class Chapter3Tests extends AnyFunSpec {

  describe("ex 3.1") {
    import Chapter3.List._
    import Chapter3._

    it("should output 3") {
      assert((List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _)))          => x
        case Nil                                   => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t)                            => h + sum(t)
        case _                                     => 101
      }) == 3)
    }
  }

  describe("List ops") {
    import Chapter3.List._
    import Chapter3._
    describe("ex 3.2 tail") {
      it("tail of non empty list should drop 1st element") {
        assert(tail(List(1, 2, 3)) == List(2, 3))
      }

      it("tail of empty list throws UnsupportedOperationException") {
        assertThrows[UnsupportedOperationException](tail(List()))
      }
    }

    describe("ex 3.3 setHead") {
      it("set head of non empty list should replace 1st element") {
        assert(setHead(List(1, 2, 3), 42) == List(42, 2, 3))
      }

      it("set head of empty list throws UnsupportedOperationException") {
        assertThrows[UnsupportedOperationException](setHead(List(), 42))
      }
    }

    describe("ex 3.4 drop") {
      it("drop on non empty list should drop `n` elements") {
        assert(drop(List(1, 2, 3), 2) == List(3))
      }

      it("drop empty list throws UnsupportedOperationException") {
        assertThrows[UnsupportedOperationException](drop(List(), 2))
      }

      it("drop of more elements than list contains throws UnsupportedOperationException") {
        assertThrows[UnsupportedOperationException](drop(List(3), 5))
      }
    }

    describe("ex 3.5 dropWhile") {
      it("drops elements as long as predicate holds") {
        assert(dropWhile[Int](List(1, 3, 2, 5, 6), _ <= 3) == List(5, 6))
      }

      it("should return list if none matched") {
        assert(dropWhile[Int](List(1, 2, 3), _ > 40) == List(1, 2, 3))
      }

      it("on empty list returns empty list") {
        assert(dropWhile[Int](Nil, _ < 44) == List())
      }
    }

    describe("ex 3.6 init") {
      it("takes all but last last element of the list") {
        assert(init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))
      }

      it("returns Nil in case of single-item list") {
        assert(init(List(1)) == Nil)
      }

      it("throws UnsupportedOperationException on empty list") {
        assertThrows[UnsupportedOperationException](init(Nil))
      }
    }
  }

}
