package part2

import part1.Chapter6
import part1.Chapter6.{RNG, SimpleRNG, State, double => rngDouble}
import part2.Chapter7.Blocking.{Par => BPar}
import part2.Chapter7.Blocking.Par.{Par => BPar}
import part2.Chapter7.NonBlocking
import part2.Chapter8.Gen.{map2LazyList, map2Option, unbounded, unit}
import part2.Chapter8.Prop.Status.{Exhausted, Proven, Unfalsified}
import part2.Chapter8.Prop._


object Chapter8 {

  case class Gen[+A](sample: State[RNG, A], exhaustive: LazyList[Option[A]]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(
      sample.flatMap(a => f(a).sample),
      exhaustive.flatMap {
        case None    => unbounded
        case Some(a) => f(a).exhaustive
      }
    )

    def map[B](f: A => B): Gen[B] = flatMap(a => unit(f(a)))

    def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
      Gen(
        sample.map2(g.sample)(f),
        map2LazyList(exhaustive, g.exhaustive)((a, b) => map2Option(a, b)(f))
      )

    def listOfN(size: Int): Gen[List[A]] =
      Gen.listOfN(size, this)

    def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (n => listOfN(n))

    def unsized: SGen[A] = Unsized(this)

    def **[B](g: Gen[B]): Gen[(A, B)] = this.map2(g)((_, _))

  }

  sealed trait SGen[+A] {
    def map[B](f: A => B): SGen[B] = this match {
      case Sized(g)   => Sized(g andThen (_ map f))
      case Unsized(g) => Unsized(g map f)
    }

    def flatMap[B](f: A => Gen[B]): SGen[B] = this match {
      case Sized(g)   => Sized(g andThen (_ flatMap f))
      case Unsized(g) => Unsized(g flatMap f)
    }

    def **[B](g2: SGen[B]): SGen[(A, B)] = (this, g2) match {
      case (Sized(g1), Sized(g2))     => Sized(n => g1(n) ** g2(n))
      case (Unsized(g1), Unsized(g2)) => Unsized(g1 ** g2)
      case (Sized(g1), Unsized(g2))   => Sized(n => g1(n) ** g2)
      case (Unsized(g1), Sized(g2))   => Sized(n => g1 ** g2(n))
    }
  }

  case class Sized[+A](g: Int => Gen[A]) extends SGen[A]

  case class Unsized[+A](get: Gen[A]) extends SGen[A]


  object Gen {
    type Domain[+A] = LazyList[Option[A]]

    def bounded[A](a: LazyList[A]): Domain[A] = a map (Some(_))

    def unbounded: Domain[Nothing] = LazyList(None)

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(Chapter6.nonNegativeInt).map(start + _ % (stopExclusive - start)), bounded(LazyList.from(start).take(stopExclusive - start)))

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a), unbounded)

    def boolean: Gen[Boolean] = Gen(State[RNG, Int](_.nextInt).map(_ % 2 == 0), bounded(LazyList(true, false)))

    def double: Gen[Double] = Gen(State(rngDouble), unbounded)

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = sequence(List.fill(n)(g))

    def listOf[A](g: Gen[A]): SGen[List[A]] = Sized(n => g.listOfN(n))

    def listOf1[A](g: Gen[A]): SGen[List[A]] = Sized(n => g.listOfN(n max 1))

    def sequence[A](as: List[Gen[A]]): Gen[List[A]] = traverse(as)(x => x)

    def traverse[A, B](as: List[A])(f: A => Gen[B]): Gen[List[B]] =
      as.foldRight(Gen.unit(List.empty[B]))((a, acc) => acc.flatMap(lst => f(a).map(b => b :: lst)))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(b => if (b) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val t = g1._2.abs / (g1._2.abs + g2._2.abs)

      double.flatMap(d => if (d <= t) g1._1 else g2._1)
    }

    def pint: Gen[BPar[Int]] = Gen.choose(-100, 100).listOfN(Gen.choose(0, 20))
      .map(list => list.foldLeft(BPar.unit(0))((p, i) => BPar.fork {
        BPar.map2(p, BPar.unit(i))(_ + _)
      }))

    def pintNonBlocking: Gen[NonBlocking.Par[Int]] = Gen.choose(-100, 100).listOfN(Gen.choose(0, 20))
      .map(list => list.foldLeft(NonBlocking.unit(0))((p, i) => NonBlocking.fork {
        NonBlocking.map2(p, NonBlocking.unit(i))(_ + _)
      }))


    def map2LazyList[A, B, C](s1: LazyList[A], s2: LazyList[B])(f: (A, B) => C): LazyList[C] =
      s1.flatMap(ll1 => s2.map(ll2 => f(ll1, ll2)))

    def map2Option[A, B, C](opt1: Option[A], opt2: Option[B])(f: (A, B) => C): Option[C] =
      opt1.flatMap(o1 => opt2.map(o2 => f(o1, o2)))
  }

  object Prop {
    type TestCases = Int
    type Max = Int
    type FailedCase = String
    type SuccessCount = Int
    type Result = Either[FailedCase, (Status, TestCases)]

    sealed trait Status

    object Status {

      case object Exhausted extends Status

      case object Proven extends Status

      case object Unfalsified extends Status

    }

    def apply(f: (TestCases, RNG) => Result): Prop =
      Prop { (_, n, rng) => f(n, rng) }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = g match {
      case Sized(g)   => forAll(g)(f)
      case Unsized(g) => forAll(g)(f)
    }


    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max

      val props: LazyList[Prop] =
        LazyList.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)

      prop.run(max, n, rng)
    }

    import scala.collection.immutable.LazyList.#::

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
      def go(i: Int, j: Int, s: LazyList[Option[A]], onEnd: Int => Result): Result =
        if (i == j) Right((Unfalsified, i))
        else s match {
          case h #:: t => h match {
            case Some(h) =>
              try {
                if (f(h)) go(i + 1, j, t, onEnd)
                else Left(h.toString)
              } catch {
                case e: Exception => Left(buildMsg(h, e))
              }
            case None    => Right((Unfalsified, i))
          }
          case _       => onEnd(i)
        }

      go(0, n / 3, as.exhaustive, i => Right((Proven, i))) match {
        case Right((Unfalsified, _)) =>
          val rands = randomStream(as)(rng).map(Some(_))
          go(n / 3, n, rands, i => Right((Unfalsified, i)))
        case s                       => s
      }
    }


    def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
      LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](a: A, exception: Exception): String =
      s"test case: $a\n" +
        s"generated an exception: ${exception.getMessage}\n" +
        s"stack trace: \n${exception.getStackTrace.mkString("\n")}"

    def runR(p: Prop,
             maxSize: Int = 100,
             testCases: Int = 100,
             rng: RNG = SimpleRNG(System.currentTimeMillis)): Result =
      p.run(maxSize, testCases, rng)

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
      runR(p, maxSize, testCases, rng) match {
        case Left(msg)               => println("! test failed: \n" + msg)
        case Right((Unfalsified, n)) => println(s"property unfalsified, ran $n tests")
        case Right((Proven, n))      => println(s"property proven, ran $n tests")
        case Right((Exhausted, n))   => println(s"unfalsified up to max size, ran $n tests")
      }

    def check(p: => Boolean): Prop = forAll(unit(()))(_ => p)

  }

  case class Prop(run: (Max, TestCases, RNG) => Result) {

    def &&(p: Prop): Prop = Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case Right((_, n)) => p.run(max, n, rng).map {
          case (s, m) => (s, +n + m)
        }
        case x             => x
      }
    }

    def ||(p: Prop): Prop = Prop {
      (max, n, rng) =>
        run(max, n, rng) match {
          case Left(msg) => p.tag(msg).run(max, n, rng)
          case r         => r
        }
    }

    def tag(msg: String): Prop = Prop {
      (max, n, rng) =>
        run(max, n, rng) match {
          case Left(e) => Left(msg + "\n" + e)
          case r       => r
        }
    }

  }

}
