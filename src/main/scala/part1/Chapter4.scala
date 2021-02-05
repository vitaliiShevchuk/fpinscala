package part1

object Chapter4 {

  case class Some[+A](value: A) extends Option[A]

  case object None extends Option[Nothing]

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(value) => Some(f(value))
      case None        => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(value) => value
      case None        => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) this else None)

    def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob
  }

  object Option {
    def apply[A](v: A): Option[A] =
      if (v == null) None
      else Some(v)

    def empty[A]: Option[A] = None

    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      for {
        a <- a
        b <- b
      } yield f(a, b)

    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight(Option(List.empty[A]))((optA, opts) => opts.flatMap(seq => optA.map(a => a :: seq)))

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a.foldRight(Option(List.empty[B])) { (a, optListOfB) =>
        optListOfB.flatMap(listOfB => f(a).map(_ :: listOfB))
      }

    def sequence2[A](a: List[Option[A]]): Option[List[A]] =
      traverse(a)(x => x)

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs)
        .map(m => xs.map(x => math.pow(x - m, 2)))
        .flatMap(mean)
    }

    def Try[A](f: => A): Option[A] =
      try {
        Some(f)
      } catch {
        case _: Exception => None
      }
  }

  sealed trait Either[+E, +A] {

    def map[B](f: A => B): Either[E, B] = this match {
      case Right(value) => Right(f(value))
      case Left(ex)     => Left(ex)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(value) => f(value)
      case Left(ex)     => Left(ex)
    }


    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(value) => Right(value)
      case _            => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        a <- this
        b <- b
      } yield f(a, b)
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {

    def mean(xs: Seq[Double]): Either[String, Double] =
      if (xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)

    def Try[A](a: => A): Either[Exception, A] =
      try {
        Right(a)
      } catch {
        case ex: Exception => Left(ex)
      }

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      es.foldRight(Right(Nil): Either[E, List[A]])((either, eitherList) => eitherList.flatMap(list => either.map(a => a :: list)))

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as.foldRight(Right(Nil): Either[E, List[B]])((a, eitherList) => eitherList.flatMap(list => f(a).map(_ :: list)))
  }

}
