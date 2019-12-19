package fplib.effects

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  def map[A,B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldLeft[F[List[A]]](unit(List()))((acc, el) => map2(acc, el)(_ :+ _))
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
  def product[A,B](ma: F[A], mb: F[B]): F[(A,B)] = map2(ma, mb)((_,_))
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldLeft[F[List[A]]](unit(List()))((acc, el) => map2(acc, f(el))((x, y) => if (y) x :+ el else x))
}

object Monad {
  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: A): Option[A] = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }
}
