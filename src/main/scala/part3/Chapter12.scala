package part3

import part3.Chapter11.Functor
import scala.collection.immutable.LazyList
import scala.util.Either
import part3.Chapter11.Monad
import java.text.SimpleDateFormat
import java.util.Date

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
      map2(fab, f)(_(_))

    def map2_[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(
        f: (A, B, C) => D
    ): F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
        f: (A, B, C, D) => E
    ): F[E] =
      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

    def map_[A, B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)

  }

  def sequence[A](as: List[LazyList[A]]): LazyList[List[A]] =
    as.foldRight(LazyList(List.empty[A]))((lla, acc) =>
      lla.flatMap(a => acc.map(b => a :: b))
    )

  def eitherMonad[E] = new Monad[({ type h[x] = Either[E, x] })#h] {
    def unit[A](a: => A): Either[E, A] = Right(a)
    def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      fa.flatMap(f)
  }

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector())
      extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  def validName(name: String): Validation[String, String] =
    if (name.nonEmpty) Success(name)
    else Failure("Name cannot be empty!")

  def validBirthDate(birthDate: String): Validation[String, Date] =
    try {
      import java.text.SimpleDateFormat
      val d: Date = new SimpleDateFormat("yyyy-MM-dd").parse(birthDate)
      Success(d): Validation[String, Date]
    } catch {
      case _: Throwable => Failure("Birthdate must be in format yyyy-MM-dd")
    }

  def validPhone(n: String): Validation[String, String] =
    if (n.matches("[0-9]{10}"))
      Success(n)
    else
      Failure("Phone number must be digits")

}
