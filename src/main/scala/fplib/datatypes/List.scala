package fplib.datatypes

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def sum(is: List[Int]): Int = is match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sumViaFold(is: List[Int]): Int = foldLeft(is)(0)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](as: List[A]): Option[List[A]] = as match {
    case Nil => None
    case Cons(h, t) => Some(t)
  }

  def setHead[A](as: List[A])(h: A): Option[List[A]] = as match {
    case Nil => None
    case Cons(_, t) => Some(Cons(h, t))
  }

  @scala.annotation.tailrec
  def drop[A](as: List[A])(n: Int): List[A] =
    if (n <= 0) as
    else as match {
      case Nil => Nil
      case Cons(_,t) => drop(t)(n-1)
    }

  @scala.annotation.tailrec
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => as
  }

  def append[A](as1: List[A])(as2: List[A]): List[A] = as1 match {
    case Nil => as2
    case Cons(h, t) => Cons(h, append(t)(as2))
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t)(z)(f))
  }

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t)(f(h, z))(f)
  }

  def length[A](as: List[A]): Int = foldLeft(as)(0)((_, b) => b + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as)(List[A]())((el, res) => Cons(el, res))
}

