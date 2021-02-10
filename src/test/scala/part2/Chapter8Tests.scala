package part2

import org.scalatest.Assertion
import org.scalatest.funspec.AnyFunSpec
import part2.Chapter7.Blocking.Par.Par
import part2.Chapter8.Gen.{choose, pint, pintNonBlocking, weighted}
import part2.Chapter8.Prop._
import part2.Chapter8.{Gen, Prop}

import java.util.concurrent.{Executors, TimeUnit}

class Chapter8Tests extends AnyFunSpec {

  def unwrap(prop: Prop): Assertion = {
    val result: Result = Prop.runR(prop)
    assert(result.isRight, result.left.getOrElse(""))
  }

  describe("Simple examples") {
    val smallInt = Gen.choose(-10, 10)
    val largeInt = Gen.choose(Int.MinValue, Int.MaxValue)

    it("no value should be bigger than max") {
      val maxProp: Prop = forAll(Gen.listOf1(smallInt)) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
      }

      unwrap(maxProp)
    }

    it("verifies list sorted") {
      val sortProp = forAll(Gen.listOf1(largeInt)) { ns =>
        !ns.sorted.sliding(2).exists {
          case x :: y :: Nil => x > y
          case _             => false
        }
      }

      unwrap(sortProp)
    }

    describe("Par props") {

      val es = Executors.newFixedThreadPool(4)

      val S = weighted(
        choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
        Gen.unit(Executors.newCachedThreadPool()) -> 0.25
      )




      describe("blocking") {
        import part2.Chapter7.Blocking.Par
        import part2.Chapter7.Blocking.Par.Par

        def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
          forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get(5, TimeUnit.SECONDS) }

        it("identity") {
          val prop = check {
            Par.equal(
              Par.map(Par.unit(1))(_ + 1),
              Par.unit(2)
            )(es).get
          }

          unwrap(prop)
        }

        it("fork") {
          val prop = forAllPar(pint)(n => Par.equal(Par.fork(n), n)) tag "fork"

          val result: Result = Prop.runR(prop)
          assert(!result.isRight, result.left.getOrElse(""))
        }


      }

      describe("non blocking") {
        import part2.Chapter7.{NonBlocking => Par}
        import part2.Chapter7.NonBlocking.Par
        import Par._

        def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
          forAll(S.map2(g)((_, _))) { case (s, a) => Par.run(s)(f(a)) }

        it("identity") {
          val prop = check {
            Par.run(es)(Par.equal(
              Par.map(Par.unit(1))(_ + 1),
              Par.unit(2)
            ))
          }

          unwrap(prop)
        }

        it("fork") {
          val prop = forAllPar(pintNonBlocking)(n => equal(Par.fork(n), n)) tag ("fork")
          unwrap(prop)
        }

      }
    }
  }

}
