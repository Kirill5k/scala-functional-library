package fplib.effects

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext

final class Fiber[A](io: IO[A]) {

  private var result: Option[A] = None
  private val callbacks: ArrayBuffer[Either[Throwable, A] => Unit] = ArrayBuffer.empty

  def start(): Unit = ExecutionContext.global.execute { () =>
    io.runAsync { a =>
      result = Some(a)
      callbacks.foreach(cb => cb(Right(a)))
    }
  }

  def join: IO[A] = IO.async { cb =>
    result match {
      case Some(value) => cb(Right(value))
      case None => callbacks += cb
    }

  }
}

sealed trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] = IO.FlatMap(this, f)

  def zip[B](that: IO[B]): IO[(A, B)] = flatMap(a => that.map(b => (a, b)))
  def map[B](f: A => B): IO[B] = flatMap[B](a => IO.pure(f(a)))
  def as[B](b: B): IO[B] = map(_ => b)

  def runAsync(register: A => Unit): Unit
  def fork: IO[Fiber[A]] = IO.Fork(this)

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
  final case class Fork[A](io: IO[A]) extends IO[Fiber[A]] {
    override def runAsync(register: Fiber[A] => Unit): Unit = {
      val fiber = new Fiber[A](io)
      fiber.start()
      register(fiber)
    }
  }

  def pure[A](value: A): IO[A] = Pure(value)
  def delay[A](f: => A): IO[A] = IO.Delay(() => f)
  def async[A](cb: (Either[Throwable, A] => Unit) => Any): IO[A] = IO.Async(cb)

}
