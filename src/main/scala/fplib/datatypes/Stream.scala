package fplib.datatypes

sealed trait Stream[+A] {
  import Stream._

  def head: Option[A] = this match {
    case EmptyStream => None
    case StreamCons(h, _) => Some(h())
  }
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
