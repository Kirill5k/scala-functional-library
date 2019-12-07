package fplib.datatypes

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {
  def sum(is: List[Int]): Int = is match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
}

