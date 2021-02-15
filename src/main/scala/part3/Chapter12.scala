package part3

import part3.Chapter11.Functor

object Chapter12 {

  trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))

    def sequence[A](as: List[F[A]]): F[List[A]] =
      traverse(as)(x => x)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(fa))

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      map2(fa, fb)((_, _))


    def apply[A, B](fab: F[A => B])(f: F[A]): F[B] =
      map2(fab, f)(_ (_))

    def map2_[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

    def map_[A, B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)

  }

  def main(args: Array[String]): Unit = {
    val f: List[Int => Int] = List((i: Int) => i * 2)
    val as: List[Int] = List(1, 2, 3)

    val value: Seq[Int => Int] = as.map(f)
    println(value)
  }

}
