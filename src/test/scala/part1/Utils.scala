package part1

import part1.Chapter6.RNG

object Utils {

  implicit class DoubleOps(val x: Double) extends AnyVal {
    def ~=(y: Double)(implicit precision: Double): Boolean = {
      if ((x - y).abs < precision) true else false
    }
  }

  case class StubRng(n: Int, step: Int = 1) extends RNG {
    override def nextInt: (Int, RNG) = (n, StubRng(n + step, step))
  }

}
