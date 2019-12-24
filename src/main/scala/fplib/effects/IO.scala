package fplib.effects

import fplib.abstractions.Monad


sealed trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
  def map[B](f: A => B): IO[B] = flatMap(f.andThen(Return(_)))
}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](sub: IO[A], f: A => IO[B]) extends IO[B]

object IO extends Monad[IO] {
  override def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
  def apply[A](a: => A): IO[A] = unit(a)

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y.flatMap(a => g(a)).flatMap(f))
    }
  }
}
