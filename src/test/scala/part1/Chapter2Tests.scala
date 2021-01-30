package part1

import org.scalatest.funspec.AnyFunSpec
import part1.Chapter2._

class Chapter2Tests extends AnyFunSpec {

  describe("exercise 2.2 Fibonacci") {
    it("should be equal to [0 1 1 2 3 5]") {
      assert((1 to 6).map(Chapter2.fib) == IndexedSeq(0, 1, 1, 2, 3, 5))
    }
  }

  describe("ex 2.2 isSorted") {
    val ordering: (Int, Int) => Boolean = _ <= _

    it("should succeed in case of Array(-4,1,2,4,6,8)") {
      val array: Array[Int] = Array(-4, 1, 2, 4, 6, 8)
      assert(isSorted(array, ordering))
    }

    it("should fail in case of Array(5,2,1,4,2,6)") {
      assert(!isSorted(Array(5, 2, 1, 4, 2, 6), ordering))
    }
  }

  describe("ex 2.3 curry can't be tested, compilation is enough") {
    assertCompiles(
      """
        |  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
        |    a => b => f(a, b)
        |
          """.stripMargin)
  }

  describe("ex 2.4 uncurry can't be tested, compilation is enough") {
    assertCompiles(
      """
        |  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
        |    (a, b) => f(a)(b)
          """.stripMargin)
  }

  describe("ex 2.5 compose") {
    it("compose(_ * 3)(_ + 1)(1) should be equal to 6 ") {
      assert(compose[Int, Int, Int](_ * 3, _ + 1)(1) == 6)
    }


  }


}
