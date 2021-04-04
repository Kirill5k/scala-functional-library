package fplib.abstractions

final case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s1) = run(s)
    (f(a), s1)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
  def zipWith[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(x => sb.map(y => f(x,y)))
  def zip[B](sb: State[S, B]): State[S, (A, B)] = zipWith(sb)((_,_))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sa.flatMap(x => sb.map(y => f(x,y)))
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldLeft[State[S, List[A]]](unit(Nil))((acc, el) => acc.flatMap(x => el.map(y => x :+ y)))
}
