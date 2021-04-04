package fplib.abstractions

trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  override def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = flatMap(fa)(a => map(fab)(f => f(a)))
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldLeft[F[List[A]]](unit(List()))((acc, el) => map2(acc, f(el))((x, y) => if (y) x :+ el else x))
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  def flatMapViaCompose[A,B](fa: F[A])(f: A => F[B]): F[B] = compose[Unit, A, B]((_: Unit) => fa, f)(())
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
  def flatMapViaMapJoin[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def forever[A,B](a: F[A]): F[B] = {
    lazy val t: F[B] = flatMap(a)(_ => t)
    t
  }
}

object Monad {
  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  type Id[A] = A
  implicit val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): A = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }
}
