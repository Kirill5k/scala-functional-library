package fplib.abstractions

import fplib.utils.{Part, Stub, WordCount}


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

  def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(implicit m: Monoid[B]): B = {
    if (as.size < 1) m.zero
    else if (as.size == 1) f(as(0))
    else {
      val (l, r) = as.splitAt(as.size/2)
      m.op(foldMap(l)(f), foldMap(r)(f))
    }
  }

  val wordCountMonoid: Monoid[WordCount] = new Monoid[WordCount] {
    override def op(a1: WordCount, a2: WordCount): WordCount = (a1, a2) match {
      case (Part(l1, c1, r1), Part(l2, c2, r2)) => Part(l1, c1+c2  + (if ((r1 + l2).isEmpty) 0 else 1) , r2)
      case (Part(l1, c1, r1), Stub(s1)) => Part(l1, c1, r1+s1)
      case (Stub(s1), Part(l1, c1, r1)) => Part(s1+l1, c1, r1)
      case (Stub(s1), Stub(s2)) => Stub(s1+s2)
    }

    override def zero: WordCount = Stub("")
  }

  def productMonoid[A,B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (a1, a2) match {
      case ((x1, y1), (x2, y2)) => (ma.op(x1, x2), mb.op(y1, y2))
    }

    override def zero: (A, B) = (ma.zero, mb.zero)
  }

  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(f1: A => B, f2: A => B): A => B = a => mb.op(f1(a), f2(a))
    override def zero: A => B = _ => mb.zero
  }
}
