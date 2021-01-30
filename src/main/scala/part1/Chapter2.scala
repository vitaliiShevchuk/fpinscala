package part1

import scala.annotation.tailrec

object Chapter2 {

  def fib(n: Int): Int = {
    if (n == 1) 0
    else if (n == 2) 1
    else fib(n - 1) + fib(n - 2)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = {
      if (i == as.length - 1) true
      else if (ordered(as(i), as(i + 1))) {
        loop(i + 1)
      } else {
        false
      }
    }

    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
