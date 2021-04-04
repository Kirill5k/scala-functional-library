package fplib.utils

import scala.annotation.tailrec

trait Rng {
  def nextInt: (Int, Rng)
}

final case class SimpleRng(seed: Long) extends Rng {
  override def nextInt: (Int, Rng) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRng(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Rng {
  type Rand[+A] = Rng => (A, Rng)

  val int: Rand[Int] = _.nextInt

  val nonNegativeInt: Rand[Int] = rng => {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  val double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  val intDouble: Rand[(Int, Double)] = rng => {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  val intDoubleViaMap2: Rand[(Int, Double)] = map2(int, double)((_, _))

  def ints(n: Int): Rand[List[Int]] = rng => {
    @tailrec
    def go(r: Rng, c: Int, is: List[Int]): (List[Int], Rng) = {
      if (c < 1) (is, r)
      else {
        val (i, r1) = int(r)
        go(r1, c-1, is :+ i)
      }
    }
    go(rng, n, Nil)
  }

  def intsViaSequence(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a,b), r2)
  }

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a,b)))

  def flatMap[A,B](r: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = r(rng)
    g(a)(r1)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_,_))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight[Rand[List[A]]](unit(Nil))((el, acc) => map2(el, acc)(_ :: _))
}
