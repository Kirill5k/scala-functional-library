package fplib.effects

object IORunner {

  def run[A](effect: IO[A]): A =
    effect match {
      case IO.Pure(value) => value
    }
}
