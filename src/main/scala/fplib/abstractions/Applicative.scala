package fplib.abstractions

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]

  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = apply(map(fa)(f.curried))(fb)
  def applyViaMap2[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldLeft[F[List[A]]](unit(List()))((acc, el) => map2(acc, el)(_ :+ _))
  def product[A,B](ma: F[A], mb: F[B]): F[(A,B)] = map2(ma, mb)((_,_))

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
}

object Applicative {
  val streamApplicative = new Applicative[LazyList] {
    override def unit[A](a: => A): LazyList[A] = LazyList.continually(a)
    override def apply[A, B](fab: LazyList[A => B])(fa: LazyList[A]): LazyList[B] = fa.zip(fab).map{ case(a, f) => f(a) }
  }
}
