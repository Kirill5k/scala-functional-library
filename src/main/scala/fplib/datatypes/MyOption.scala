package fplib.datatypes

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case None => None
    case Some(x) => f(x)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case None => ob
    case oa => oa
  }
  def filter(f: A => Boolean): MyOption[A] = this match {
    case Some(x) if f(x) => Some(x)
    case _ => None
  }
}
case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

object MyOption {
  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f
  def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    a.flatMap(x => b.map(y => f(x,y)))
  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] =
    a.foldLeft[MyOption[List[A]]](Some(List[A]()))((acc, el) => map2(acc, el)(_ :+ _))
  def traverse[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    as.foldLeft[MyOption[List[B]]](Some(List[B]()))((acc, el) => map2(acc, f(el))(_ :+ _))
  def sequenceViaTraverse[A](a: List[MyOption[A]]): MyOption[List[A]] = traverse(a)(identity)
}

