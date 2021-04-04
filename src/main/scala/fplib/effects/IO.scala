package fplib.effects

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future, Promise}

final class Fiber[A](io: IO[A]) {

  private var result: Option[Either[Throwable, A]]                 = None
  private val callbacks: ArrayBuffer[Either[Throwable, A] => Unit] = ArrayBuffer.empty

  def start(): Unit = ExecutionContext.global.execute { () =>
    io.runAsync { a =>
      result = Some(a)
      callbacks.foreach(cb => cb(a))
    }
  }

  def join: IO[A] = IO.async { cb =>
    result match {
      case Some(value) => cb(value)
      case None        => callbacks += cb
    }

  }
}

sealed trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] = IO.FlatMap(this, f)

  def zip[B](that: IO[B]): IO[(A, B)] = flatMap(a => that.map(b => (a, b)))
  def map[B](f: A => B): IO[B]        = flatMap[B](a => IO.pure(f(a)))
  def as[B](b: B): IO[B]              = map(_ => b)

  def runAsync(register: Either[Throwable, A] => Unit): Unit
  def fork: IO[Fiber[A]] = IO.Fork(this)
  def zipPar[B](that: IO[B]): IO[(A, B)] = for {
    fa <- this.fork
    fb <- that.fork
    a  <- fa.join
    b  <- fb.join
  } yield (a, b)

  def toFuture: Future[A] = {
    val p = Promise[A]()
    runAsync { cb =>
      cb.fold(p.failure, p.success)
      ()
    }
    p.future
  }
}

object IO {
  final private case class Pure[A](a: A) extends IO[A] {
    override def runAsync(register: Either[Throwable, A] => Unit): Unit =
      register(Right(a))
  }
  final private case class Map[A, B](ioa: IO[A], f: A => B) extends IO[B] {
    override def runAsync(register: Either[Throwable, B] => Unit): Unit =
      ioa.runAsync(va => register(va.fold(e => Left(e), a => Right(f(a)))))
  }
  final private case class FlatMap[A, B](ioa: IO[A], f: A => IO[B]) extends IO[B] {
    override def runAsync(register: Either[Throwable, B] => Unit): Unit =
      ioa.runAsync {
        case Left(e)  => register(Left(e))
        case Right(a) => f(a).runAsync(register)
      }
  }
  final private case class Delay[A](f: () => A) extends IO[A] {
    override def runAsync(register: Either[Throwable, A] => Unit): Unit =
      register(Right(f()))
  }
  final private case class Async[A](cb: (Either[Throwable, A] => Unit) => Any) extends IO[A] {
    override def runAsync(register: Either[Throwable, A] => Unit): Unit = cb(register)
  }
  final private case class Fork[A](io: IO[A]) extends IO[Fiber[A]] {
    override def runAsync(register: Either[Throwable, Fiber[A]] => Unit): Unit = {
      val fiber = new Fiber[A](io)
      fiber.start()
      register(Right(fiber))
    }
  }

  def apply[A](f: => A): IO[A]                                   = Delay(() => f)
  def delay[A](f: => A): IO[A]                                   = Delay(() => f)
  def pure[A](value: A): IO[A]                                   = Pure(value)
  def async[A](cb: (Either[Throwable, A] => Unit) => Any): IO[A] = Async(cb)
}
