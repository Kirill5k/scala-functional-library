package fplib.datatypes

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](h: A, t: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def sum(is: MyList[Int]): Int = is match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sumViaFold(is: MyList[Int]): Int = foldLeft(is)(0)(_ + _)

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def productViaFold(ds: MyList[Double]): Double = foldLeft(ds)(1.0)(_ * _)

  def tail[A](as: MyList[A]): MyOption[MyList[A]] = as match {
    case Nil => None
    case Cons(_, t) => Some(t)
  }

  def setHead[A](as: MyList[A])(h: A): MyOption[MyList[A]] = as match {
    case Nil => None
    case Cons(_, t) => Some(Cons(h, t))
  }

  @scala.annotation.tailrec
  def drop[A](as: MyList[A])(n: Int): MyList[A] =
    if (n <= 0) as
    else as match {
      case Nil => Nil
      case Cons(_,t) => drop(t)(n-1)
    }

  @scala.annotation.tailrec
  def dropWhile[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => as
  }

  def append[A](as1: MyList[A], as2: MyList[A]): MyList[A] = as1 match {
    case Nil => as2
    case Cons(h, t) => Cons(h, append(t, as2))
  }

  def appendViaFold[A](as1: MyList[A])(as2: MyList[A]): MyList[A] = foldRight(as1)(as2)( Cons(_, _))

  def init[A](as: MyList[A]): MyList[A] = as match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: MyList[A])(z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs)(z)(f))
  }

  @scala.annotation.tailrec
  def foldLeft[A, B](as: MyList[A])(z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t)(f(z, h))(f)
  }

  def foldLeftViaFoldRight[A, B](as: MyList[A])(z: B)(f: (B, A) => B): B =
    foldRight(as)((b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def length[A](as: MyList[A]): Int = foldLeft(as)(0)((b, _) => b + 1)

  def reverse[A](as: MyList[A]): MyList[A] = foldLeft(as)(MyList[A]())((acc, el) => Cons(el, acc))

  def flatten[A](l: MyList[MyList[A]]): MyList[A] = foldRight(l)(MyList[A]())(append)

  def addOne(is: MyList[Int]): MyList[Int] = foldRight(is)(MyList[Int]())((el, acc) => Cons(el+1, acc))
  def addOneViaMap(is: MyList[Int]): MyList[Int] = map(is)(_+1)

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] = foldRight(as)(MyList[B]())((el, acc) => Cons(f(el), acc))

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(_, xs) => filter(xs)(f)
  }

  def filterViaFold[A](as: MyList[A])(f: A => Boolean): MyList[A] = foldRight(as)(MyList[A]())((el, acc) => if (f(el)) Cons(el, acc) else acc)

  def filterViaFlatMap[A](as: MyList[A])(f: A => Boolean): MyList[A] = flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = flatten(map(as)(f))

  def zipWith[A, B, C](as: MyList[A], bs: MyList[B])(f: (A, B) => C): MyList[C] = (as, bs) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }
}

