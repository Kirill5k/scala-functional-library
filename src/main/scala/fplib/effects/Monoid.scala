package fplib.effects


trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}


object Monoid {
  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 :++ a2
    override def zero: List[A] = Nil
  }

  val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplicationMonoid: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  def optionMonoid[A](implicit m: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.flatMap(x => a2.map(y => m.op(x, y)))
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(f: A => A, g: A => A): A => A = f andThen g
    override def zero: A => A = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def concatenate[A](as: List[A])(implicit m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
  def foldMap[A, B](as: List[A])(f: A => B)(implicit m: Monoid[B]): B = concatenate(as.map(f))

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B)(implicit m: Monoid[B]): B =
    foldMap[A, B => B](as)(a => b => f(b, a))(endoMonoid[B])(z)
}
