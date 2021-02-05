package part1

import org.scalatest.funspec.AnyFunSpec

class Chapter5Tests extends AnyFunSpec {

  import Chapter5.Stream

  describe("Stream ops") {

    val stream = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)

    def throwingStream(n: Int, throwOn: Int): Stream[Int] = {
      Stream(LazyList.from(1).map(n => {
        if (n == throwOn)
          throw new IllegalStateException(s"shouldn't evaluate $n")
        n
      }).take(n): _*)
    }

    describe("ex 5.1 toList") {
      it("must return list") {
        assert(stream.toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
      }
    }

    describe("ex 5.2 take") {
      it("must take n elements") {
        val stream = throwingStream(10, 6).take(5)
        assert(stream.toList == List(1, 2, 3, 4, 5))
      }

      it("must return an Empty on Empty") {
        assert(Stream.empty[Int].take(2) == Stream.empty)
      }
    }

    describe("ex 5.3 takeWhile") {
      it("must take elements until non-matching element occurred") {
        val stream = throwingStream(10, 7).takeWhile(_ <= 5)
        assert(stream.toList == List(1, 2, 3, 4, 5))
      }

      it("must return empty if empty stream was passed") {
        val stream = Stream.empty[Int].takeWhile(_ > 15)
        assert(stream.toList == Nil)
      }

      describe("ex 5.5 takeWhile") {
        it("must take elements until non-matching element occurred") {
          val stream = throwingStream(10, 7).takeWhile2(_ <= 5)
          assert(stream.toList == List(1, 2, 3, 4, 5))
        }

        it("must return empty if empty stream was passed") {
          val stream = Stream.empty[Int].takeWhile2(_ > 15)
          assert(stream.toList == Nil)
        }
      }
    }

    describe("ex 5.4 forAll") {
      it("verifies that all elements match predicate") {
        val stream = throwingStream(20, 21)
        assert(stream.forAll(_ <= 20))
      }

      it("must not evaluate after first non matching element") {
        val stream = throwingStream(21, 12)
        assert(!stream.forAll(_ <= 10))
      }

      describe("forAll2") {
        it("verifies that all elements match predicate") {
          val stream = throwingStream(20, 21)
          assert(stream.forAll2(_ <= 20))
        }

        it("must not evaluate after first non matching element") {
          val stream = throwingStream(21, 12)
          assert(!stream.forAll2(_ <= 10))
        }
      }
    }

    describe("ex 5.6 headOption using foldRight") {
      it("must return None on empty stream") {
        assert(Stream.empty[Int].headOption.isEmpty)
      }

      it("must return head element without evaluating others") {
        assert(throwingStream(2, 2).headOption.contains(1))
      }

      describe("headOption2") {
        it("must return None on empty stream") {
          assert(Stream.empty[Int].headOption2.isEmpty)
        }

        it("must return head element without evaluating others") {
          assert(throwingStream(2, 2).headOption2.contains(1))
        }
      }
    }

    describe("ex 5.7") {
      describe("map") {
        it("must map elements using f") {
          stream.map(_ * 2).toList == (1 to 9).map(_ * 2).toList
        }
      }

      describe("filter") {
        it("must filter elements using f") {
          assert(stream.filter(_ <= 5).toList == (1 to 5).toList)
        }
      }

      describe("append") {
        it("must concatenate two streams") {
          assert(stream.append(stream).toList == (1 to 9).concat(1 to 9).toList)
        }
      }

      describe("flatMap") {
        it("must flatten stream") {
          assert(stream.flatMap(x => Stream(x, x)).toList == (1 to 9).toList.flatMap(x => List(x, x)))
        }
      }
    }

    describe("find") {
      it("must stop after match found") {
        val stream = throwingStream(10, 6)
        assert(stream.find(_ == 5).contains(5))
      }
    }

    describe("ex 5.8") {
      describe("constant") {
        it("must emit inf stream of 5") {
          assert(Stream.constant(5).take(1000).toList == List.fill(1000)(5))
        }
      }

      describe("ex 5.12") {
        it("must emit inf stream of 5") {
          assert(Stream.constant2(5).take(1000).toList == List.fill(1000)(5))
        }
      }
    }

    describe("ex 5.9") {
      describe("from") {
        it("must emit inf stream strating from 3") {
          assert(Stream.from(3).take(1000).toList == List(3 to 1002: _*))
        }
      }

      describe("ex 5.12") {
        it("must emit inf stream strating from 3") {
          assert(Stream.from2(3).take(1000).toList == List(3 to 1002: _*))
        }
      }
    }

    describe("ex 5.10") {
      describe("fibs") {
        it("must emit Fibonacci stream") {
          assert(Stream.fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
        }
      }

      describe("ex 5.12") {
        describe("fibs") {
          it("must emit Fibonacci stream") {
            assert(Stream.fibs2.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
          }
        }

      }
    }

    describe("ex 5.13") {
      describe("zipAll") {
        it("must zip two streams") {
          val (s1, s2) = (throwingStream(5, 10), throwingStream(3, 10))
          assert(s1.zipAll(s1).toList == List(1,2,3,4,5).map(Option(_)).zipAll(List(1,2,3,4,5).map(Option(_)), None, None))
        }

        it("must zip two streams replacing missing right values with None") {
          val (s1, s2) = (throwingStream(5, 10), throwingStream(3, 10))
          assert(s1.zipAll(s2).toList == List(1,2,3,4,5).map(Option(_)).zipAll(List(1,2,3).map(Option(_)), None, None))
        }

        it("must zip two streams replacing missing left values with None") {
          val (s1, s2) = (throwingStream(5, 10), throwingStream(3, 10))
          assert(s2.zipAll(s1).toList == List(1,2,3).map(Option(_)).zipAll(List(1,2,3,4,5).map(Option(_)), None, None))
        }
      }
    }

    describe("ex 5.14") {
      describe("startsWith") {
        it("must return false if s2 is prefix of s1") {
          assert(!Stream(1,2,3,4).startsWith(Stream(2,3)))
        }

        it("must return true if s2 eq s1") {
          assert(Stream(1,2,3,4).startsWith(Stream(1,2,3,4)))
        }

        it("must return false if s2 is empty") {
          assert(!Stream(1,2,3,4).startsWith(Stream.empty))
        }

      }
    }

    describe("ex 5.15") {
      describe("tails") {
        it("must return tails of Stream") {
          Stream(1,2,3,4).tails.map(_.toList).toList == List(1,2,3,4).tails.toList
        }
      }
    }

    describe("hasSubsequence implementations") {
      it("hasSubsequence") {
        assert(Stream(1,2,3,4).hasSubsequence(Stream(3,4)))
      }

      it("hasSubsequence non matching") {
        assert(!Stream(1,2,3,4).hasSubsequence(Stream(4, 5)))
      }

      it("hasSubsequence2") {
        assert(Stream(1,2,3,4).hasSubsequence2(Stream(3,4)))
      }

      it("hasSubsequence2 non matching") {
        assert(!Stream(1,2,3,4).hasSubsequence2(Stream(4, 5)))
      }

    }

    describe("scanRight") {
      it("returns stream of intermediate results") {
        assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
      }
    }

    describe("misc") {
      describe("zip") {
        it("must zip and trim to same length") {
          assert(Stream(1,2,3,4).zip(Stream(1,2)).toList == List(1,2,3,4).zip(List(1,2)))
        }
      }
    }
  }

}
