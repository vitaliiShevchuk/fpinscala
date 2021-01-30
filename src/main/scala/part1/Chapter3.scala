package part1

import scala.annotation.tailrec

object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](h: A, t: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Cons(h, t) => h + sum(t)
      case Nil        => 0
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(h, t)   => h * product(t)
    }

    def apply[A](as: A*): List[A] = {
      @tailrec
      def loop(l: List[A], i: Int, as: Seq[A]): List[A] = {
        if (as.isEmpty || i == as.length)
          l
        else
          loop(Cons(as(i), l), i + 1, as)
      }

      loop(Nil, 0, as.reverse)
    }

    def tail[A](l: List[A]): List[A] = l match {
      case Nil           => throw new UnsupportedOperationException("tail of empty list")
      case Cons(_, tail) => tail
    }

    def setHead[A](l: List[A], a: A): List[A] = l match {
      case Nil           => throw new UnsupportedOperationException("setHead of empty list")
      case Cons(_, tail) => Cons(a, tail)
    }

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] =
      if (n == 0) l
      else l match {
        case Nil  => throw new UnsupportedOperationException("drop on empty list")
        case list => drop(tail(list), n - 1)
      }

    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      l match {
        case Cons(h, tail) if f(h) => dropWhile(tail, f)
        case _                     => l
      }
    }

    def append[A](l: List[A], r: List[A]): List[A] = l match {
      case Nil           => r
      case Cons(h, tail) => Cons(h, append(tail, r))
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil           => throw new UnsupportedOperationException("init of Nil")
      case Cons(_, Nil)  => Nil
      case Cons(h, tail) => Cons(h, init(tail))
    }
  }

}
