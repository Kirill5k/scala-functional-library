package fplib.datatypes

sealed trait Stream[+A] {
  import Stream._

  def head: Option[A] = this match {
    case EmptyStream => None
    case StreamCons(h, _) => Some(h())
  }
  def headViaFold: Option[A] = foldRight[Option[A]](None)((el, _) => Some(el))
  def toList: List[A] = this match {
    case EmptyStream => EmptyList
    case StreamCons(h, t) => ListCons(h(), t().toList)
  }
  def take(n: Int): Stream[A] =
    if (n < 1) empty
    else this match {
      case EmptyStream => empty
      case StreamCons(h, t) => cons(h(), t().take(n-1))
    }
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case StreamCons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty
  }
  def takeWhileViaFold(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((el, acc) => if(f(el)) cons(el, acc) else empty)
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case StreamCons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  def exists(f: A => Boolean): Boolean = this match {
    case StreamCons(h, t) => f(h()) || t().exists(f)
    case _ => false
  }
  def existsViaFold(f: A => Boolean): Boolean = foldRight(false)((a, b) => f(a) || b)
  def forAll(f: A => Boolean): Boolean = this match {
    case StreamCons(h, t) => f(h()) && t().forAll(f)
    case EmptyStream => true
  }
  def map[B](f: A => B): Stream[B] = this match {
    case EmptyStream => empty
    case StreamCons(h, t) => cons(f(h()), t().map(f))
  }
  def mapViaFold[B](f: A => B): Stream[B] = foldRight[Stream[B]](empty)((el, acc) => cons(f(el), acc))
  def append[B>:A](s: => Stream[B]): Stream[B] = this match {
    case EmptyStream => s
    case StreamCons(h, t) => cons(h(), t().append(s))
  }
  def appendViaFold[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((el, acc) => cons(el, acc))
  def filter(f: A => Boolean): Stream[A] = this match {
    case EmptyStream => empty
    case StreamCons(h, t) if f(h()) => cons(h(), t().filter(f))
    case StreamCons(_, t) => t().filter(f)
  }
  def filterViaFold(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((el, acc) => if (f(el)) cons(el, acc) else acc)
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => f(h) append t)
  def find(f: A => Boolean): Option[A] = filter(f).head
}
case object EmptyStream extends Stream[Nothing]
case class StreamCons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def fibs(): Stream[BigInt] = {
    def go(n1: => BigInt, n2: => BigInt): Stream[BigInt] = {
      cons(n1, go(n2, n1+n2))
    }
    go(0,1)
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => empty
  }
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(x => Some((x,x)))
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(x => Some((x, x+1)))
  def fibsViaUnfold(): Stream[BigInt] = unfold[BigInt, (BigInt, BigInt)]((0, 1)){ case (x, y) => Some((x, (y, x+y)))}

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    StreamCons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = EmptyStream

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) EmptyStream else cons(as.head, apply(as.tail: _*))
}
