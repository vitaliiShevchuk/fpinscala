package part1

import org.scalatest.funspec.AnyFunSpec
import part1.Chapter3.{Branch, Leaf}

import scala.math.BigDecimal.double2bigDecimal


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

    describe("ex 3.7 short-circuiting product via foldRight") {
      describe("can't be short-circuited because foldRight doesn't contains only edge and recursion cases") {
      }
    }

    describe("ex 3.8 foldRight with List constructors") {
      it("does nothing, because list is simply reconstructed starting from last element") {
        assert(List(1, 2, 3, 4, 5) == List.foldRight(List(1, 2, 3, 4, 5), Nil: List[Int])(Cons(_, _)))
      }
    }

    describe("ex 3.9") {
      it("computes the length of empty list as 0") {
        assert(List.length(Nil) == 0)
      }

      it("computes the length of non empty list correctly") {
        assert(List.length(List(1, 2, 3)) == 3)
      }
    }

    describe("ex 3.10 evidence that foldRight is not stack-safe") {
      it("throws stack overflow exception") {
        val l = List(1 to 1000_000)

        assertThrows[StackOverflowError](List.foldRight(l, 0)(_ + _))
      }
    }

    describe("ex 3.11") {
      it("sum results are the same") {
        val list = List(1 to 100)
        assert(List.sum2(list) == List.sum3(list))
      }

      it("product results are the same") {
        val list = List(1 to 100)
        assert(List.length(list) == List.length2(list))
      }

      it("length results are the same") {
        val list = List(1.0, 2.0, 3.0, 4.0)
        assert(List.product2(list) == List.product3(list))
      }

    }

    describe("ex 3.12") {
      it("foldLeft using List constructors is reverse") {
        assert(List(1, 2, 3, 4, 5) == List.reverse(List(5, 4, 3, 2, 1)))
      }
    }

    describe("ex 3.13") {
      it("foldRightViaFoldLeft shouldn't reverse the list") {
        val l = List(1 to 100)
        assert(List.foldRightViaFoldLeft(l, Nil: List[Int])(Cons(_, _)) == l)
      }

      it("foldLeftViaFoldRight should reverse the list") {
        val l = List(1 to 100)
        assert(List.foldLeftViaFoldRight(l, Nil: List[Int])((acc, a) => Cons(a, acc)) == List.reverse(l))
      }

    }

    describe("ex 3.14") {
      it("appends via foldRight") {
        assert(List.foldRightAppend(List(1 to 3), List(4 to 8)) == List(1 to 8))
      }

      describe("append via foldLeft reverses left list and appends right list to the end") {
        it("appends via foldLeft") {
          assert(List.foldLeftAppend(List(1 to 3), List(4 to 8)) == List(1 to 8))
        }
      }
    }

    describe("ex 3.15") {
      it("joins list of lists into single list") {
        val l = List(List(1, 2), List(3, 4), List(5, 6))
        assert(List.concat(l) == List(1 to 6))
      }
    }

    describe("ex 3.16") {
      it("adds one to each element") {
        assert(List(2 to 6) == List.addOne(List(1 to 5)))
      }
    }

    describe("ex 3.17") {
      it("converts list of double to list of string") {
        val ds = List(1.0, 2.0, 2.2, 3.3, 4.4)
        assert(List.doubleToString(ds) == List("1.0", "2.0", "2.2", "3.3", "4.4"))
      }
    }

    describe("ex 3.18 map") {
      it("should apply given function to each element in the list without changing it's structure") {
        assert(List.map(List(1 to 10))(_ * 2) == List(2 to 20 by 2))
      }
    }

    describe("ex 3.19 filter") {
      it("should exclude elements using given predicate") {
        assert(List.filter(List(1 to 10))(_ % 2 == 0) == List(2, 4, 6, 8, 10))
      }
    }

    describe("ex 3.20 flatMap") {
      it("should flatten nested lists") {
        val l = List(List(1, 2), List(3, 4), List(5, 6))
        assert(List.flatMap(l)(x => x) == List(1 to 6))
      }

      it("should join and flatten") {
        val l = List(1, 2, 3)
        assert(List.flatMap(l)(x => List(x, x)) == List(1, 1, 2, 2, 3, 3))
      }
    }

    describe("ex 3.21 filterViaFlatMap should work just as filter") {
      it("should exclude elements using given predicate") {
        assert(List.filterViaFlatMap(List(1 to 10))(_ % 2 == 0) == List(2, 4, 6, 8, 10))
      }
    }

    describe("ex 3.22") {
      it("add corresponding list elements") {
        assert(List.addCorrespondingElements(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
      }
    }

    describe("ex 3.22 zipWith") {
      it("zips two lists into list of tuples") {
        assert(List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ -> _) == List(1 -> 4, 2 -> 5, 3 -> 6))
      }
    }

    describe("ex 3.23 hasSubsequence") {
      it("should return true for List(1,2,3,4) and List(2,3)") {
        assert(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
      }

      it("should return false for List(1,2,3,4) and List(1,3)") {
        assert(!List.hasSubsequence(List(1, 2, 3, 4), List(1, 3)))
      }
    }
  }

  describe("Tree ops") {
    import Chapter3.Tree
    /*
    *             B
    *           /   \
    *         B      B
    *       /  \    / \
    *      2    5  42  B
    *                 / \
    *               11   B
    *                   / \
    *                  1  66
    * */

    val tree = Branch(
      Branch(Leaf(2), Leaf(5)),
      Branch(Leaf(42), Branch(
        Leaf(11), Branch(
          Leaf(1), Leaf(66)
        )
      ))
    )

    describe("size") {
      it("must be 11") {
        assert(Tree.size(tree) == 11)
      }

      it("using fold  should match") {
        assert(Tree.size(tree) == Tree.fold(tree)(_ => 1)(_ + _ + 1))
      }
    }

    describe("max") {
      it("must be 66") {
        assert(Tree.maximum(tree) == 66)
      }

      it("using fold should match") {
        assert(Tree.maximum(tree) == Tree.fold(tree)(x => x)(_.max(_)))
      }
    }

    describe("depth") {
      it("must be 5") {
        assert(Tree.depth(tree) == 5)
      }

      it("using fold should match") {
        assert(Tree.fold(tree)(_ => 1)(1 + _.max(_)) == Tree.depth(tree))
      }
    }

    describe("map") {
      it("all leaf nodes must be doubled") {
        val expected = Branch(
          Branch(Leaf(4), Leaf(10)),
          Branch(Leaf(84), Branch(
            Leaf(22), Branch(
              Leaf(2), Leaf(132)
            )
          ))
        )

        assert(Tree.map(tree)(_ * 2) == expected)
      }

      it("using fold should match") {
        assert(Tree.fold(tree)(value => Leaf(value * 2): Tree[Int])((l, r) => Branch(l, r)) == Tree.map(tree)(_ * 2))
      }
    }

  }

}
