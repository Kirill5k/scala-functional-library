package fplib.abstractions

trait Traverse[F[_]] extends Functor[F] {
  def traverse[M[_]:Applicative,A,B](fa: F[A])(f: A => M[B]): M[F[B]]
  def sequence[M[_]:Applicative,A](fma: F[M[A]]): M[F[A]] = traverse(fma)(ma => ma)
  def map[A,B](fa: F[A])(f: A => B): F[B] = traverse[Monad.Id, A, B](fa)(f)(Monad.idMonad)
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[M[_] : Applicative, A, B](fa: List[A])(f: A => M[B]): M[List[B]] = {
      val M = implicitly[Applicative[M]]
      fa.foldLeft[M[List[B]]](M.unit(List[B]()))((acc, a) => M.map2(acc, f(a))(_:+_))
    }
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_] : Applicative, A, B](oa: Option[A])(f: A => M[B]): M[Option[B]] = {
      val M = implicitly[Applicative[M]]
      oa match {
        case None => M.unit(None)
        case Some(a) => M.map(f(a))(Some(_))
      }
    }
  }
}
