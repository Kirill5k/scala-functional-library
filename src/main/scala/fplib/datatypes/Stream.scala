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
    foldRight[Stream[A]](empty)((el, acc) => if(f(el)) cons(el, acc) else acc)
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
}
case object EmptyStream extends Stream[Nothing]
case class StreamCons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    StreamCons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = EmptyStream

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) EmptyStream else cons(as.head, apply(as.tail: _*))
}
