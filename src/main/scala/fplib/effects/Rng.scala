package fplib.effects

import scala.annotation.tailrec

trait Rng {
  def nextInt: (Int, Rng)
}

case class SimpleRng(seed: Long) extends Rng {
  override def nextInt: (Int, Rng) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRng(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Rng {
  def nonNegativeInt(rng: Rng): (Int, Rng) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: Rng): (Double, Rng) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: Rng): ((Int, Double), Rng) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def ints(n: Int)(rng: Rng): (List[Int], Rng) = {
    @tailrec
    def go(r: Rng, c: Int, is: List[Int]): (List[Int], Rng) = {
      if (c < 1) (is, r)
      else {
        val (i, r1) = r.nextInt
        go(r1, c-1, i :: is)
      }
    }
    go(rng, n, Nil)
  }


}
