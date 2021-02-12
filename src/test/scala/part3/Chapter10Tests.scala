package part3

import org.scalatest.funspec.AnyFunSpec
import part1.Chapter3.{Branch, Leaf}
import part2.Chapter7.NonBlocking
import part2.Chapter7.NonBlocking.Par
import part2.Chapter8.{Gen, Prop}
import part3.Chapter10.Foldable.{IndexedSeqFoldable, LazyListFoldable, OptionFoldable, TreeFoldable}
import part3.Chapter10.{Foldable, Monoid}
import java.util.concurrent.Executors

import scala.util.Random

class Chapter10Tests extends AnyFunSpec {

  describe("Monoid ops") {

    describe("ex 10.1") {
      import Chapter10.Monoid.{boolAnd, boolOr, intAddition, intMultiplication}

      it("intAddition") {
        assert(List(1, 2, 3, 4, 5).foldLeft(intAddition.zero)(intAddition.op) == 15)
      }

      it("intMultiplication") {
        assert(List(1, 2, 3, 4, 5).foldLeft(intMultiplication.zero)(intMultiplication.op) == 120)
      }

      it("boolOr") {
        assert(List(true, false).foldLeft(boolOr.zero)(boolOr.op))
      }

      it("boolAnd") {
        assert(List(true, true).foldLeft(boolAnd.zero)(boolAnd.op))
      }

    }

    describe("ex 10.2") {
      import part3.Chapter10.Monoid.optionMonoid
      it("optionMonoid") {
        val m = optionMonoid[Boolean]
        assert(List(Some(true), None).foldLeft(m.zero)(m.op).contains(true))
      }
    }

    describe("ex 10.3") {
      import part3.Chapter10.Monoid.endoMonoid
      it("endoMonoid") {
        val m: Monoid.Monoid[Int => Int] = endoMonoid[Int]
        val x: Int => Int = _ + 1
        val y: Int => Int = _ + 2
        assert(List(x, y).foldLeft(m.zero)(m.op)(0) == 3)
      }
    }

    describe("ex 10.4") {
      it("monoidLaws") {
        assert(Prop.runR(Monoid.monoidLaws(Monoid.intAddition, Gen.choose(-1000, 1000))).isRight)
      }
    }

    describe("ex 10.5") {
      it("foldMap") {
        assert(Monoid.foldMap(List(1, 2, 3, 4, 5), Monoid.intAddition)(x => x) == 15)
      }
    }

    describe("ex 10.6") {
      it("foldRightViaFolMap") {
        assert(Monoid.foldRightViaFoldMap(List(1, 2, 3, 4, 5), 0)(_ + _) == 15)
      }
    }

    describe("ex 10.7") {
      it("foldMapV") {
        assert(Monoid.foldMapV(IndexedSeq(1, 2, 3, 4, 5), Monoid.intAddition)(x => x) == 15)
      }
    }

    describe("ex 10.8") {
      it("parFoldMap") {
        val par: Par[Int] = Monoid.parFoldMap((1 to 1000).toIndexedSeq, Monoid.intAddition)(x => x)

        val result: Int = NonBlocking.run(Executors.newFixedThreadPool(2))(par)
        assert(result == ((1000 * 1001) / 2))
      }
    }

    describe("ex 10.9") {
      it("ordering") {
        assert(Monoid.ordered(1 to 1000))
      }

      it("ordering fails") {
        assert(!Monoid.ordered(1000 to 1 by -1))
      }

    }

    describe("ex 10.10 & 10.11") {
      it("wc") {
        assert(Monoid.countWords("lorem ipsum dolor sit amet, ") == 5)
      }
    }

    describe("ex 10.16") {
      it("product monoid") {
        val l = List(1, 2, 3, 4, 5)

        val M: Monoid.Monoid[(Int, Int)] = Monoid.productMonoid(Monoid.intAddition, Monoid.intMultiplication)

        assert(Foldable.ListFoldable.foldMap(l)(a => (a, a))(M) == (15, 120))
      }

    }

    describe("ex 10.17") {
      it("mapMergeMonoid") {

        val M: Monoid.Monoid[Map[String, Map[String, Int]]] = Monoid.mapMergeMonoid(Monoid.mapMergeMonoid(Monoid.intAddition))

        val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 4))
        val m2: Map[String, Map[String, Int]] = Map("o1" -> Map("i2" -> 3))

        val m3 = M.op(M.op(M.zero, m1), m2)

        assert(m3 == Map("o1" -> Map("i1" -> 1, "i2" -> 7)))
      }
    }

    describe("ex 10.18") {
      it("functionMonoid") {
        val M: Monoid.Monoid[Int => Int] = Monoid.functionMonoid(Monoid.intAddition)
        val l = List((_: Int) + 1, (_: Int) + 2, (_: Int) + 3)
        val f = Foldable.ListFoldable.concatenate(l)(M)

        assert(f(10) == 36)
      }
    }

    describe("ex 10.19") {
      it("bag") {
        val rands = LazyList.continually(Random.nextInt()).take(1000).toIndexedSeq
        val result: Map[Int, Int] = Monoid.bag(rands)

        assert(result == rands.groupMap(x => x)(x => x).view.mapValues(_.size).toMap)
      }
    }


  }

  describe("Foldable ops") {
    describe("ex 10.12") {

      it("foldable concat") {
        assert(LazyListFoldable.concatenate(LazyList(1, 2, 3))((Monoid.intMultiplication)) == 6)
      }

      describe("foldable List") {
        import Foldable.ListFoldable
        it("foldRight") {
          assert(ListFoldable.foldRight(List(1, 2, 3, 4, 5))(0)(_ + _) == 15)
        }

        it("foldLeft") {
          assert(ListFoldable.foldLeft(List(1, 2, 3, 4, 5))(0)(_ + _) == 15)
        }

        it("foldMap") {
          assert(ListFoldable.foldMap(List("1", "2", "3", "4", "5"))(_.toInt)(Monoid.intAddition) == 15)
        }

        it("toList") {
          assert(ListFoldable.toList(List(1, 2, 3)) == List(1, 2, 3))
        }
      }

      describe("foldable IndexedSeq") {
        it("foldRight") {
          assert(IndexedSeqFoldable.foldRight(IndexedSeq(1, 2, 3, 4, 5))(0)(_ + _) == 15)
        }

        it("foldLeft") {
          assert(IndexedSeqFoldable.foldLeft(IndexedSeq(1, 2, 3, 4, 5))(0)(_ + _) == 15)
        }

        it("foldMap") {
          assert(IndexedSeqFoldable.foldMap(IndexedSeq("1", "2", "3", "4", "5"))(_.toInt)(Monoid.intAddition) == 15)
        }

        it("toList") {
          assert(IndexedSeqFoldable.toList(IndexedSeq(1, 2, 3)) == List(1, 2, 3))
        }
      }

      describe("foldable LazyList") {
        it("foldRight") {
          assert(LazyListFoldable.foldRight(LazyList(1, 2, 3, 4, 5))(0)(_ + _) == 15)
        }

        it("foldLeft") {
          assert(LazyListFoldable.foldLeft(LazyList(1, 2, 3, 4, 5))(0)(_ + _) == 15)
        }

        it("foldMap") {
          assert(LazyListFoldable.foldMap(LazyList("1", "2", "3", "4", "5"))(_.toInt)(Monoid.intAddition) == 15)
        }

        it("toList") {
          assert(LazyListFoldable.toList(LazyList(1, 2, 3)) == List(1, 2, 3))
        }
      }

      describe("ex 10.13 foldable Tree") {
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

        it("foldRight") {
          assert(TreeFoldable.foldRight(tree)(0)(_ + _) == 127)
        }

        it("foldLeft") {
          assert(TreeFoldable.foldLeft(tree)(0)(_ + _) == 127)
        }

        it("foldMap") {
          assert(TreeFoldable.foldMap(tree)(x => x)(Monoid.intAddition) == 127)
        }

        it("toList") {
          assert(TreeFoldable.toList(tree) == List(66, 1, 11, 42, 5, 2))
        }
      }

      describe("ex 10.14 foldable Option") {
        it("foldRight") {
          assert(OptionFoldable.foldRight(Option(1))(0)(_ + _) == 1)
        }

        it("foldRight over None") {
          assert(OptionFoldable.foldRight(Option.empty[Int])(0)(_ + _) == 0)
        }


        it("foldLeft") {
          assert(OptionFoldable.foldLeft(Option(1))(0)(_ + _) == 1)
        }

        it("foldLeft over None") {
          assert(OptionFoldable.foldLeft(Option.empty[Int])(0)(_ + _) == 0)
        }


        it("foldMap") {
          assert(OptionFoldable.foldMap(Option("1"))(_.toInt)(Monoid.intAddition) == 1)
        }

        it("foldMap over None") {
          assert(OptionFoldable.foldMap(Option.empty[Int])(_.toInt)(Monoid.intAddition) == 0)
        }


        it("toList") {
          assert(OptionFoldable.toList(Option(1)) == List(1))
        }
      }


    }
  }

}
