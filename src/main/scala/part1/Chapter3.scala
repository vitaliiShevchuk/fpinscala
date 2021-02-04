package part1

import part1.Chapter3.List.foldLeft

import scala.annotation.tailrec

object Chapter3 {

  sealed trait List[+A] {
    override def toString: String = {
      val listOfStrings = List.map(this)(_.toString)
      "[" + foldLeft(listOfStrings, "")(_ + "," + _) + "]"
    }
  }

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

    def apply(range: Range): List[Int] =
      apply(range: _*)

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

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      as match {
        case Nil        => z
        case Cons(h, t) => f(h, foldRight(t, z)(f))
      }
    }

    def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

    def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

    def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil        => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

    def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

    def length2[A](ns: List[A]): Int = foldLeft(ns, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] = {
      foldLeft(as, Nil: List[A])((acc, a) => Cons(a, acc))
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    def foldRightAppend[A](l: List[A], r: List[A]): List[A] =
      foldRight(l, r)(Cons(_, _))

    def foldLeftAppend[A](l: List[A], r: List[A]): List[A] =
      foldLeft(List.reverse(l), r)((acc, a) => Cons(a, acc))

    def concat[A](list: List[List[A]]): List[A] =
      foldRight(list, Nil: List[A])(append)

    def addOne(ns: List[Int]): List[Int] = ns match {
      case Nil        => Nil
      case Cons(h, t) => Cons(h + 1, addOne(t))
    }

    def doubleToString(ds: List[Double]): List[String] = ds match {
      case Nil        => Nil
      case Cons(h, t) => Cons(h.toString, doubleToString(t))
    }

    def map[A, B](l: List[A])(f: A => B): List[B] = l match {
      case Nil        => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case Nil        => Nil
      case Cons(h, t) =>
        if (f(h))
          Cons(h, filter(t)(f))
        else
          filter(t)(f)
    }

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
      case Nil        => Nil
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    }

    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(a => if (f(a)) List(a) else Nil)

    def addCorrespondingElements(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
      case (Cons(a, as), Cons(b, bs)) => Cons(a + b, addCorrespondingElements(as, bs))
      case (Nil, Nil)                 => Nil
      case _ => ???
    }

    def zipWith[A, B](l: List[A], r: List[A])(f: (A, A) => B): List[B] = (l, r) match {
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
      case (Nil, Nil)                 => Nil
      case _ => ???
    }

    @tailrec
    def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Cons(a, as), Cons(b, bs)) if a == b => startsWith(as, bs)
      case _ => false
    }

    @tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, as) =>  hasSubsequence(as, sub)
    }

  }


  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_)         => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left).max(maximum(right))
    }

    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    def fold[A, B](tree: Tree[A])(l: A => B)(b: (B, B) => B): B = tree match {
      case Leaf(value)         => l(value)
      case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }
  }
}
