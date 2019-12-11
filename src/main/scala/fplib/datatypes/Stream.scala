package fplib.datatypes

sealed trait Stream[+A] {
  import Stream._

  def head: Option[A] = this match {
    case EmptyStream => None
    case StreamCons(h, _) => Some(h())
  }
  def headViaFold: Option[A] = foldRight[Option[A]](None)((el, _) => Some(el))
  def toList: List[A] = this match {
    case EmptyStream => EmptyList
    case StreamCons(h, t) => ListCons(h(), t().toList)
  }
  def take(n: Int): Stream[A] =
    if (n < 1) empty
    else this match {
      case EmptyStream => empty
      case StreamCons(h, t) => cons(h(), t().take(n-1))
    }
  def takeViaUnfold(n: Int): Stream[A] = unfold[A, (Stream[A], Int)]((this, n)){
    case (_, c) if c < 1 => None
    case (StreamCons(h, t), c) => Some((h(), (t(), c-1)))
    case (EmptyStream, _) => None
  }
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case StreamCons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty
  }
  def takeWhileViaFold(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((el, acc) => if(f(el)) cons(el, acc) else empty)
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = unfold(this){
    case StreamCons(h, t) if f(h()) => Some((h(), t()))
    case _ => None
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case StreamCons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  def exists(f: A => Boolean): Boolean = this match {
    case StreamCons(h, t) => f(h()) || t().exists(f)
    case _ => false
  }
  def existsViaFold(f: A => Boolean): Boolean = foldRight(false)((a, b) => f(a) || b)
  def forAll(f: A => Boolean): Boolean = this match {
    case StreamCons(h, t) => f(h()) && t().forAll(f)
    case EmptyStream => true
  }
  def map[B](f: A => B): Stream[B] = this match {
    case EmptyStream => empty
    case StreamCons(h, t) => cons(f(h()), t().map(f))
  }
  def mapViaFold[B](f: A => B): Stream[B] = foldRight[Stream[B]](empty)((el, acc) => cons(f(el), acc))
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold[B, Stream[A]](this) {
    case StreamCons(h, t) => Some((f(h()), t()))
    case EmptyStream => None
  }
  def append[B>:A](s: => Stream[B]): Stream[B] = this match {
    case EmptyStream => s
    case StreamCons(h, t) => cons(h(), t().append(s))
  }
  def appendViaFold[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((el, acc) => cons(el, acc))
  def filter(f: A => Boolean): Stream[A] = this match {
    case EmptyStream => empty
    case StreamCons(h, t) if f(h()) => cons(h(), t().filter(f))
    case StreamCons(_, t) => t().filter(f)
  }
  def filterViaFold(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((el, acc) => if (f(el)) cons(el, acc) else acc)
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => f(h) append t)
  def find(f: A => Boolean): Option[A] = filter(f).head
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)){
    case (StreamCons(h1, t1), StreamCons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (EmptyStream, StreamCons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
    case (StreamCons(h1, t1), EmptyStream) => Some(((Some(h1()), None), (t1(), empty)))
    case (EmptyStream, EmptyStream) => None
  }
  def startsWith[A](s2: Stream[A]): Boolean = zipAll(s2).foldRight(true){
    case ((Some(h1), Some(h2)), acc) if h1 == h2 && acc => true
    case ((_, None), acc) if acc => true
    case _ => false
  }
  def tails: Stream[Stream[A]] = unfold(this){
    case x @ StreamCons(_, t) => Some((x, t()))
    case EmptyStream => None
  }.append(Stream(empty))
  def hasSubsequence[A](s: Stream[A]): Boolean = tails.exists(_.startsWith(s))
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = unfold(this){
    case s @ StreamCons(_, t) => Some(s.foldRight(z)(f), t())
    case EmptyStream => None
  }.append(Stream(z))
}
case object EmptyStream extends Stream[Nothing]
case class StreamCons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def fibs(): Stream[BigInt] = {
    def go(n1: => BigInt, n2: => BigInt): Stream[BigInt] = {
      cons(n1, go(n2, n1+n2))
    }
    go(0,1)
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => empty
  }
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(x => Some((x,x)))
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(x => Some((x, x+1)))
  def fibsViaUnfold(): Stream[BigInt] = unfold[BigInt, (BigInt, BigInt)]((0, 1)){ case (x, y) => Some((x, (y, x+y)))}

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    StreamCons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = EmptyStream

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) EmptyStream else cons(as.head, apply(as.tail: _*))
}
