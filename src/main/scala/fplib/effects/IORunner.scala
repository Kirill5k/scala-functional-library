package fplib.effects

object IORunner {

  def run[A](effect: IO[A]): A =
    effect match {
      case IO.Pure(value) => value
      case IO.Zip(a, b) => (run(a), run(b))
      case IO.Map(a, f) => f(run(a))
      case IO.Delay(f) => f()
      case IO.FlatMap(a, f) => run(f(run(a)))
    }
}
