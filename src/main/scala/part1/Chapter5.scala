package part1

import scala.annotation.tailrec

object Chapter5 {

  sealed trait Stream[+A] {

    import Stream._

    def toList: List[A] = {
      @tailrec
      def loop(l: List[A], s: Stream[A]): List[A] = s match {
        case Empty      => l
        case Cons(h, t) => loop(h() :: l, t())
      }

      loop(Nil, this).reverse
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _                    => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _                   => this
    }

    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f.apply(h()) => cons(h(), t().takeWhile(f))
      case _                          => empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

    def exists(f: A => Boolean): Boolean =
      foldRight(false)((a, b) => f(a) || b)

    def forAll(f: A => Boolean): Boolean =
      !exists(!f(_))

    def forAll2(f: A => Boolean): Boolean =
      foldRight(true)((a, b) => f(a) && b)

    def takeWhile2(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else empty)

    def headOption: Option[A] = this match {
      case Empty      => None
      case Cons(h, _) => Some(h())
    }

    def headOption2: Option[A] = foldRight(Option.empty[A])((a, _) => Option(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B])((a, b) => cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((a, b) => if (f(a)) cons(a, b) else b)

    def append[B >: A](b: => Stream[B]): Stream[B] =
      foldRight(b)((a, b) => cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B])((a, b) => f(a).append(b))

    def find(p: A => Boolean): Option[A] =
      filter(p).headOption

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _                            => None
      }

    def zip[B](s2: Stream[B]): Stream[(A, B)] =
      zipWith(s2)((_, _))

    def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      unfold((this, s2)) {
        case (Empty, Empty)               => None
        case (Cons(h, t), Empty)          => Some(f(Some(h()), Option.empty[B]) -> ((t(), empty[B])))
        case (Empty, Cons(h, t))          => Some(f(Option.empty[A], Some(h())) -> (empty[A], t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))

      }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWithAll(s2)((_, _))

    def startsWith[B >: A](s: Stream[B]): Boolean = {
      if (s == Empty) false
      else zipAll(s).takeWhile(_._2.isDefined) forAll2 {
        case (h1, h2) => h1 == h2
      }
    }

    def hasSubsequence[B >: A](s: Stream[B]): Boolean = this match {
      case Empty                   => s == Empty
      case _ if this.startsWith(s) => true
      case Cons(_, t)              => t() hasSubsequence s
    }

    def tails: Stream[Stream[A]] = {
      unfold(this) {
        case Empty => None
        case s     => Option((s, s.drop(1)))
      } append Stream(empty)
    }

    def hasSubsequence2[B >: A](s: Stream[B]): Boolean = {
      val tails1 = tails
      println(tails.map(_.toList).toList)
      tails1 exists (_ startsWith s)
    }

    def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
      foldRight((z, Stream(z))) { (a, b) =>
        lazy val prevResult = b

        val a2 = f(a, prevResult._1)
        (a2, cons(a2, prevResult._2))
      }._2
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val h = hd
      lazy val t = tl
      Cons(() => h, () => t)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def loop(a: => Int, b: => Int): Stream[Int] = {
        cons(a, loop(b, a + b))
      }

      loop(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) map {
        case (a, s) => cons(a, unfold(s)(f))
      } getOrElse empty[A]

    def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

    def from2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

    def fibs2: Stream[Int] = unfold((0, 1)) {
      case (a, b) => Some(a, (b, a + b))
    }
  }

}
