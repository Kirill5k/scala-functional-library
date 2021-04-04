package fplib.abstractions

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
  def unzip[A,B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
}

object Functor {
  implicit val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
}
