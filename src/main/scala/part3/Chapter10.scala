package part3

import part1.Chapter3
import part1.Chapter3.Tree
import part2.Chapter7.NonBlocking.Par
import part2.Chapter7.{NonBlocking => Par}
import part2.Chapter8._

import scala.annotation.tailrec

object Chapter10 {

  object Monoid {

    trait Monoid[A] {
      def op(a1: A, a2: A): A

      def zero: A
    }

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.foldRight(m.zero)((a, b) => m.op(f(a), b))

    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
      if (v.length <= 1)
        v.headOption.map(a => m.op(f(a), m.zero)).getOrElse(m.zero)
      else {
        val (l, r) = v.splitAt(v.length / 2)
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
      }

    def foldRightViaFoldMap[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldMap(as, endoMonoid[B])(f.curried)(z)

    def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
      foldMapV(v, par(m))(Par.asyncF(f))

    def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
      override def zero: Par[A] = Par.unit(m.zero)
      override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    }

    def ordered(ints: IndexedSeq[Int]): Boolean = {
      val orderingM: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
        def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = (a1, a2) match {
          case (Some((x, y, b)), Some((x1, y1, b1))) =>
            Some((x min x1, y max y1, b && b1 && y < x1))
          case (x, None)                             => x
          case (None, x)                             => x
        }

        def zero: Option[(Int, Int, Boolean)] = None
      }

      foldMapV(ints, orderingM)(i => Some((i, i, true))).forall(_._3)
    }

    sealed trait WC

    case class Stub(chars: String) extends WC

    case class Part(lStub: String, words: Int, rStub: String) extends WC

    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      override def op(a1: WC, a2: WC): WC = (a1, a2) match {
        case (Stub(chars1), Stub(chars2))                              => Stub(chars1 + chars2)
        case (Stub(chars), Part(lStub, words, rStub))                  => Part(chars + lStub, words, rStub)
        case (Part(lStub, words, rStub), Stub(chars))                  => Part(lStub, words, rStub + chars)
        case (Part(lStub, words, rStub), Part(lStub1, words1, rStub1)) =>
          Part(lStub, words + (if ((rStub + lStub1).isEmpty) 0 else 1) + words1, rStub1)
      }
      override def zero: WC = Stub("")
    }

    def countWords(s: String): Int = {

      def wc(c: Char): WC =
        if (c.isWhitespace)
          Part("", 0, "")
        else
          Stub(c.toString)

      def unstub(s: String): Int = s.length min 1

      foldMapV(s, wcMonoid)(wc) match {
        case Stub(chars)               => unstub(chars)
        case Part(lStub, words, rStub) => unstub(lStub) + words + unstub(rStub)
      }
    }

    val intAddition: Monoid[Int] = new Monoid[Int] {
      val zero: Int = 0
      override def op(a1: Int, a2: Int): Int = a1 + a2
    }

    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      val zero: Int = 1
      override def op(a1: Int, a2: Int): Int = a1 * a2
    }

    val boolOr: Monoid[Boolean] = new Monoid[Boolean] {
      val zero: Boolean = false
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 | a2
    }

    val boolAnd: Monoid[Boolean] = new Monoid[Boolean] {
      val zero: Boolean = true
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    }

    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
      override def zero: Option[A] = None
    }

    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      override def op(a1: A => A, a2: A => A): A => A = a2 compose a1
      override def zero: A => A = x => x
    }

    def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      override def zero: (A, B) = (A.zero, B.zero)
    }

    def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
      def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero)((m, key) => {
          m.updated(key, V.op(a1.getOrElse(key, V.zero), a2.getOrElse(key, V.zero)))
        })

      def zero: Map[K, V] = Map.empty[K, V]
    }

    def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))
      override def zero: A => B = _ => B.zero
    }

    def bag[A](as: IndexedSeq[A]): Map[A, Int] =
      foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))

    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
      val identityLaw: Prop = Prop.forAll(gen) { a =>
        m.op(m.zero, a) == a
      }

      val associativityLaw: Prop = Prop.forAll(gen ** gen ** gen) {
        case ((x, y), z) => m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
      }

      identityLaw && associativityLaw
    }

  }

  object Foldable {

    trait Foldable[F[_]] {

      import Monoid.Monoid

      def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

      def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

      def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B

      def toList[A](f: F[A]): List[A] =
        foldRight(f)(List.empty[A])((a, acc) => a :: acc)

      def concatenate[A](as: F[A])(m: Monoid[A]): A =
        foldLeft(as)(m.zero)(m.op)
    }

    object ListFoldable extends Foldable[List] {
      def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
      def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
      def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid.Monoid[B]): B =
        foldLeft(as)(m.zero)((b, a) => m.op(f(a), b))

      override def toList[A](f: List[A]): List[A] = f
    }

    object IndexedSeqFoldable extends Foldable[IndexedSeq] {
      def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
      def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
      def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid.Monoid[B]): B =
        foldLeft(as)(m.zero)((b, a) => m.op(f(a), b))
    }

    object LazyListFoldable extends Foldable[LazyList] {
      def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
      def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
      def foldMap[A, B](as: LazyList[A])(f: A => B)(m: Monoid.Monoid[B]): B =
        foldLeft(as)(m.zero)((b, a) => m.op(f(a), b))
    }

    object TreeFoldable extends Foldable[Tree] {
      import Chapter3.{Leaf, Tree, Branch}

      override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid.Monoid[B]): B = as match {
        case Leaf(a)         => f(a)
        case Branch(left, right) => m.op(foldMap(left)(f)(m), foldMap(right)(f)(m))
      }

      override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
        foldMap(as)(f.curried)(Monoid.endoMonoid)(z)

      override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
        foldMap(as)(a => (b: B) => f(b, a))(Monoid.endoMonoid)(z)
    }

    object OptionFoldable extends Foldable[Option] {
      def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
        case Some(value) => f(value, z)
        case None        => z
      }
      def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
        case Some(value) => f(z, value)
        case None        => z
      }
      def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid.Monoid[B]): B = as match {
        case Some(value) => f(value)
        case None        => m.zero
      }

    }


  }

}
