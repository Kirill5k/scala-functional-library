package fplib.data

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](as: Tree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(is: Tree[Int]): Int = is match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def depth[A](as: Tree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  def map[A, B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def flatMap[A, B](as: Tree[A])(f: A => Tree[B]): Tree[B] = as match {
    case Leaf(x) => f(x)
    case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
  }

  def fold[A,B](as: Tree[A])(f: A => B)(g: (B,B) => B): B = as match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def mapViaFold[A, B](as: Tree[A])(f: A => B): Tree[B] = fold(as)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
