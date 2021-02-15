package part3

import part1.Chapter6.State
import part2.Chapter7.NonBlocking
import part2.Chapter7.NonBlocking.Par
import part2.Chapter8.Gen
import part3.Chapter12.Applicative

object Chapter11 {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(fab: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A, B](ef: Either[F[A], F[B]]): F[Either[A, B]] = ef match {
      case Left(value)  => map(value)(Left(_))
      case Right(value) => map(value)(Right(_))
    }

  }

  trait Monad[F[_]] extends Applicative[F]{
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map2[A, B, C](fa: F[A], fb: F[B])(fab: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => fab(a, b)))

    override def sequence[A](lma: List[F[A]]): F[List[A]] =
      lma.foldRight(unit(List.empty[A]))((fa, acc) => flatMap(fa)(a => map(acc)(a :: _)))

    override def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
      la.foldRight(unit(List.empty[B]))((a, acc) => flatMap(f(a))(b => map(acc)(l => b :: l)))

    override def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))

    override def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      ms.foldRight(unit(List.empty[A]))((a, acc) => flatMap(f(a))(b => map(acc)(l => if (b) a :: l else l)))

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
      flatMap(f(a))(g(_))

    def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
      compose((_: Unit) => fa, f)(())

    def join[A](ffa: F[F[A]]): F[A] =
      flatMap(ffa)(a => a)

    def flatMapViaJoin[A, B](fa: F[A])(f: A => F[B]): F[B] =
      join(map(fa)(f))

    def composeViaJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => join(map(f(a))(g))

  }

  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(fab: A => B): List[B] = fa.map(fab)
  }

  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa flatMap f
  }


  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = NonBlocking.unit(a)
    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = NonBlocking.flatMap(fa)(f)
  }


  val optMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
  }


  def stateMonad[S]: Monad[({type h[A] = State[S, A]})#h] = new Monad[({type h[A] = State[S, A]})#h] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)
    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f
  }

  type Id[A] = A

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

  type Kleisli[A, B, F[_]] = A => F[B]

  type Reader[R, A] = Kleisli[R, A, Id]

  def readerMonad[R] = new Monad[({type h[x] = Reader[R, x]})#h] {
    override def unit[A](a: => A): Reader[R, A] = _ => a
    override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      a => f(fa(a))(a)
  }

}
