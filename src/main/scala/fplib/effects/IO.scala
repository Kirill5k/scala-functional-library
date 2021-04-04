package fplib.effects

sealed trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] = IO.FlatMap(this, f)

  def zip[B](that: IO[B]): IO[(A, B)] = flatMap(a => that.map(b => (a, b)))
  def map[B](f: A => B): IO[B] = flatMap[B](a => IO.pure(f(a)))
  def as[B](b: B): IO[B] = map(_ => b)

  def runAsync(register: A => Unit): Unit
}

object IO {
  final case class Pure[A](value: A) extends IO[A] {
    override def runAsync(register: A => Unit): Unit = register(value)
  }
  final case class Zip[A, B](a: IO[A], b: IO[B]) extends IO[(A, B)] {
    override def runAsync(register: ((A, B)) => Unit): Unit = a.runAsync(va => b.runAsync(vb => register((va, vb))))
  }
  final case class Map[A, B](a: IO[A], f: A => B) extends IO[B] {
    override def runAsync(register: B => Unit): Unit = a.runAsync(vb => register(f(vb)))
  }
  final case class FlatMap[A, B](a: IO[A], f: A => IO[B]) extends IO[B] {
    override def runAsync(register: B => Unit): Unit = a.runAsync(va => f(va).runAsync(register))
  }
  final case class Delay[A](f: () => A) extends IO[A] {
    override def runAsync(register: A => Unit): Unit = register(f())
  }
  final case class Async[A](cb: (Either[Throwable, A] => Unit) => Any) extends IO[A] {
    override def runAsync(register: A => Unit): Unit = cb {
      case Right(value) => register(value)
      case Left(error) => println(s"error in async callback $error")
    }
  }

  def pure[A](value: A): IO[A] = Pure(value)
  def delay[A](f: => A): IO[A] = IO.Delay(() => f)
  def async[A](cb: (Either[Throwable, A] => Unit) => Any): IO[A] = IO.Async(cb)

}
