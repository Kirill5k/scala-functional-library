package fplib.effects

import scala.collection.immutable.List

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(implicit mb: Monoid[B]): B

  def concatenate[A](as: F[A])(implicit m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](as: F[A]): List[A] = foldRight[A, List[A]](as)(List[A]())(_ :: _)
}

object Foldable {
  val foldableList: Foldable[List] = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight[B](z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: A => B)(implicit mb: Monoid[B]): B = foldLeft(as.map(f))(mb.zero)(mb.op)
  }
}
