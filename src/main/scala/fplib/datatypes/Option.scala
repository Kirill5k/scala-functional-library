package fplib.datatypes

import scala.collection.immutable.List

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case oa => oa
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => Some(x)
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x,y)))
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldLeft[Option[List[A]]](Some(List[A]()))((acc, el) => map2(acc, el)(_ :+ _))
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldLeft[Option[List[B]]](Some(List[B]()))((acc, el) => map2(acc, f(el))(_ :+ _))
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
}

