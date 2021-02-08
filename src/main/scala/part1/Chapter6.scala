package part1

object Chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0X5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt

    (if (i < 0) -(i+1) else i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)

    (i / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    List.fill(count)(0).foldRight((List.empty[Int], rng)) {
      case (_, (list, rng)) =>
        val (i, rng2) = rng.nextInt
        (i :: list, rng2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  val double2: Rand[Double] = map(nonNegativeInt) {
    case i if i == Int.MaxValue => i - 1 / Int.MaxValue
    case i                      => i
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a: A, rng2: RNG) = f(rng)
    val randB: Rand[B] = g(a)
    val (b: B, rng3) = randB(rng2)
    (b, rng3)
  }

  def sequence[A](l: List[Rand[A]]): Rand[List[A]] =
    l.foldRight(unit(List.empty[A]))((randA, randListA) => map2(randA, randListA)((a, listA) => a :: listA))

  def mapViaFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] = flatMap(ra)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>

      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
  }

  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] = State(s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
      val (a, s1) = this.run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }

  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](list: List[State[S, A]]): State[S, List[A]] =
      list.foldRight(State.unit[S, List[A]](List.empty[A]))((st, acc) => st.flatMap(a => acc.map(b => a :: b)))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield s

  }

  object MachineSimulation {

    sealed trait Input

    case object Coin extends Input

    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int)

    def action(input: Input): Machine => Machine = st =>
      (input, st) match {
        case (_, Machine(_, 0, _))                              => st
        case (Coin, Machine(false, _, _))                       => st
        case (Turn, Machine(true, _, _))                        => st
        case (Coin, Machine(locked, candies, coins)) if locked  => Machine(locked = false, candies = candies, coins = coins + 1)
        case (Turn, Machine(locked, candies, coins)) if !locked => Machine(locked = true, candies = candies - 1, coins)
      }


    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      for {
        _ <- State.sequence(inputs.map(action _ andThen State.modify[Machine]))
        s <- State.get[Machine]
      } yield (s.coins, s.candies)
    }

  }

}
