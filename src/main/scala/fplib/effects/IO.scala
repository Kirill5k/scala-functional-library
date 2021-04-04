package fplib.effects

sealed trait IO[A]

object IO {
  final case class Pure[A](value: A) extends IO[A]

  def pure[A](value: A): IO[A] = Pure(value)
}
