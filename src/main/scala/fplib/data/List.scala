package fplib.data

sealed trait List[+A]
case object EmptyList extends List[Nothing]
case class ListCons[+A](h: A, t: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = if (as.isEmpty) EmptyList else ListCons(as.head, apply(as.tail: _*))

  def sum(is: List[Int]): Int = is match {
    case EmptyList => 0
    case ListCons(x, xs) => x + sum(xs)
  }

  def sumViaFold(is: List[Int]): Int = foldLeft(is)(0)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case EmptyList => 1.0
    case ListCons(0.0, _) => 0.0
    case ListCons(x, xs) => x * product(xs)
  }

  def productViaFold(ds: List[Double]): Double = foldLeft(ds)(1.0)(_ * _)

  def tail[A](as: List[A]): Option[List[A]] = as match {
    case EmptyList => None
    case ListCons(_, t) => Some(t)
  }

  def setHead[A](as: List[A])(h: A): Option[List[A]] = as match {
    case EmptyList => None
    case ListCons(_, t) => Some(ListCons(h, t))
  }

  @scala.annotation.tailrec
  def drop[A](as: List[A])(n: Int): List[A] =
    if (n <= 0) as
    else as match {
      case EmptyList => EmptyList
      case ListCons(_,t) => drop(t)(n-1)
    }

  @scala.annotation.tailrec
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case ListCons(h, t) if f(h) => dropWhile(t)(f)
    case _ => as
  }

  def append[A](as1: List[A], as2: List[A]): List[A] = as1 match {
    case EmptyList => as2
    case ListCons(h, t) => ListCons(h, append(t, as2))
  }

  def appendViaFold[A](as1: List[A])(as2: List[A]): List[A] = foldRight(as1)(as2)( ListCons(_, _))

  def init[A](as: List[A]): List[A] = as match {
    case EmptyList => EmptyList
    case ListCons(_, EmptyList) => EmptyList
    case ListCons(h, t) => ListCons(h, init(t))
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as match {
    case EmptyList => z
    case ListCons(x, xs) => f(x, foldRight(xs)(z)(f))
  }

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
    case EmptyList => z
    case ListCons(h, t) => foldLeft(t)(f(z, h))(f)
  }

  def foldLeftViaFoldRight[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldRight(as)((b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def length[A](as: List[A]): Int = foldLeft(as)(0)((b, _) => b + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as)(List[A]())((acc, el) => ListCons(el, acc))

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l)(List[A]())(append)

  def addOne(is: List[Int]): List[Int] = foldRight(is)(List[Int]())((el, acc) => ListCons(el+1, acc))
  def addOneViaMap(is: List[Int]): List[Int] = map(is)(_+1)

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as)(List[B]())((el, acc) => ListCons(f(el), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case EmptyList => EmptyList
    case ListCons(x, xs) if f(x) => ListCons(x, filter(xs)(f))
    case ListCons(_, xs) => filter(xs)(f)
  }

  def filterViaFold[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as)(List[A]())((el, acc) => if (f(el)) ListCons(el, acc) else acc)

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) ListCons(a, EmptyList) else EmptyList)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (EmptyList, _) | (_, EmptyList) => EmptyList
    case (ListCons(x, xs), ListCons(y, ys)) => ListCons(f(x, y), zipWith(xs, ys)(f))
  }
}

