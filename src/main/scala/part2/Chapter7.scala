package part2

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch}


object Chapter7 {

  object Blocking {

    object Par {

      import java.util.concurrent.{ExecutorService, Future, TimeUnit}

      type Par[A] = ExecutorService => Future[A]

      private case class UnitFuture[A](get: A) extends Future[A] {
        override def isDone: Boolean = true

        override def get(timeout: Long, unit: TimeUnit): A = get

        override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

        override def isCancelled: Boolean = false
      }

      private case class Map2Future[A, B, C](a: Future[A], b: Future[B])(f: (A, B) => C) extends Future[C] {
        @volatile private var state: Option[C] = None

        override def isDone: Boolean = state.isDefined

        override def isCancelled: Boolean = a.isCancelled || b.isCancelled

        override def cancel(interrupt: Boolean): Boolean = a.cancel(interrupt) || b.cancel(interrupt)

        override def get(): C = doGet(Long.MaxValue)

        override def get(timeout: Long, unit: TimeUnit): C = doGet(unit.toNanos(timeout))

        private def doGet(timeoutInNanos: Long): C = state match {
          case Some(value) => value
          case None        =>
            val start = System.nanoTime
            val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
            val stop = System.nanoTime
            val aTime = stop - start

            val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
            val ret = f(ar, br)
            state = Some(ret)
            ret
        }
      }

      def unit[A](a: A): Par[A] = _ => UnitFuture(a)

      def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => {
        val af = pa(es)
        val bf = pb(es)
        Map2Future(af, bf)(f)
      }

      def map[A, B](pa: Par[A])(f: A => B): Par[B] =
        map2(pa, unit(()))((a, _) => f(a))

      def asyncF[A, B](f: A => B): A => Par[B] = a =>
        lazyUnit(f(a))

      def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        ps.foldRight(Par.unit(List.empty[A]))((a, acc) => map2(a, acc)(_ :: _))

      def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val af: A => Par[B] = asyncF(f)
        val fs: List[Par[B]] = ps.map(af)
        sequence(fs)
      }

      def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
        val value = as.map(asyncF((a: A) => if (f(a)) List(a) else List.empty[A]))
        map(sequence(value))(_.flatten)
      }

      def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get)

      def delay[A](a: => Par[A]): Par[A] = es => a(es)

      def lazyUnit[A](a: => A): Par[A] = fork(unit(a))


      def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] = es =>
        f(p(es).get)(es)

      def join[A](a: Par[Par[A]]): Par[A] = es =>
        a(es).get()(es)

      def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

      def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
        Par.map2(p, p2)(_ == _)
    }


  }

  object NonBlocking {

    import java.util.concurrent.ExecutorService

    sealed trait Future[+A] {
      private[NonBlocking] def apply(cb: A => Unit): Unit
    }

    type Par[+A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = _ => new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = flatMap(pa)(a => unit(f(a)))

    def sequence[A](pa: List[Par[A]]): Par[List[A]] =
      pa.foldRight(unit(List.empty[A]))((a, acc) => map2(a, acc)(_ :: _))

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val af: A => Par[B] = asyncF(f)
      val fs: List[Par[B]] = ps.map(af)
      sequence(fs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val value = as.map(asyncF((a: A) => if (f(a)) List(a) else List.empty[A]))
      map(sequence(value))(_.flatten)
    }

    //call r on another tread
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        override def call(): Unit = r
      })

    //after receiving cb - submit a to be called on another tread
    def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
      def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
    }

    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
      map2(p, p2)(_ == _)

    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case Some(b) => eval(es)(cb(f(a, b)))
            case None    => ar = Some(a)
          }

          case Right(b) => ar match {
            case Some(a) => eval(es)(cb(f(a, b)))
            case None    => br = Some(b)
          }
        }

        pa(es)(a => combiner ! Left(a))
        pb(es)(b => combiner ! Right(b))
      }
    }

    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] = es => new Future[B] {
      def apply(cb: B => Unit): Unit = p(es)(a => f(a)(es)(cb))
    }

    def join[A](pp: Par[Par[A]]): Par[A] = es => new Future[A] {
      def apply(cb: A => Unit): Unit = pp(es)(p => eval(es) {
        p(es)(cb)
      })
    }


    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a =>
        ref.set(a)
        latch.countDown()
      }
      latch.await()
      ref.get()
    }
  }

}
