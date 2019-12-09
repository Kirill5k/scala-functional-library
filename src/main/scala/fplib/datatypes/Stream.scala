package fplib.datatypes

sealed trait Stream[+A]
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
