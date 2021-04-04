package fplib.effects

sealed trait IO[A] {

  def zip[B](that: IO[B]): IO[(A, B)] = IO.Zip(this, that)
  def map[B](f: A => B): IO[B] = IO.Map(this, f)
  def flatMap[B](f: A => IO[B]): IO[B] = IO.FlatMap(this, f)
  def as[B](b: B): IO[B] = map(_ => b)
}

object IO {
  final case class Pure[A](value: A) extends IO[A]
  final case class Zip[A, B](a: IO[A], b: IO[B]) extends IO[(A, B)]
  final case class Map[A, B](a: IO[A], f: A => B) extends IO[B]
  final case class FlatMap[A, B](a: IO[A], f: A => IO[B]) extends IO[B]
  final case class Delay[A](f: () => A) extends IO[A]

  def pure[A](value: A): IO[A] = Pure(value)
  def delay[A](f: => A): IO[A] = IO.Delay(() => f)
}
