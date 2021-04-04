package fplib.effects

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.Future
import java.lang.Thread

class IOSpec extends AsyncWordSpec with Matchers {

  "An IO" should {

    "return pure value" in {
      IO.pure(42).toFuture().map(_ mustBe 42)
    }

    "zip 2 values together" in {
      IO.pure(42).zip(IO.pure("42")).toFuture().map(_ mustBe (42, "42"))
    }

    "map value inside" in {
      IO.pure(42).map(_ * 2).toFuture().map(_ mustBe 84)
    }

    "suspend computation" in {
      var suspended = true
      val io        = IO.delay { suspended = false }

      suspended mustBe true
      io.toFuture().map{ res =>
        res mustBe ()
        suspended mustBe false
      }
    }

    "flat map values" in {
      val io = IO
        .delay(println("starting io chain"))
        .as(42)
        .flatMap(a => IO.delay(println("doing computation")).as(2).flatMap(b => IO.delay(a * b)))

      io.toFuture().map(_ mustBe 84)
    }

    "run async computations" in {
      val io = IO.async[Int] { cb =>
        Future {
          println("starting computation")
          Thread.sleep(1000)
          cb(Right(42))
          println("completed computation")
        }
      }

      io.toFuture().map(_ mustBe 42)
    }

    "run computations in parallel" in {
      def computation: IO[Int] = IO.delay {
        println("starting computation")
        val result = List.fill(9999)(7).sum
        println("completed computation")
        result
      }

      val result = for {
        f1   <- computation.fork
        f2   <- computation.fork
        _    <- IO.delay(println("..."))
        res1 <- f1.join
        res2 <- f2.join
      } yield res1 + res2

      result.toFuture().map(_ mustBe 139986)
    }
  }
}
