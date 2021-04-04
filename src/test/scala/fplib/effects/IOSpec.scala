package fplib.effects

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IOSpec extends AnyWordSpec with Matchers {

  "An IO" should {

    "return pure value" in {
      IO.pure(42).runAsync(_ mustBe 42)
    }

    "zip 2 values together" in {
      IO.pure(42).zip(IO.pure("42")).runAsync(_ mustBe (42, "42"))
    }

    "map value inside" in {
      IO.pure(42).map(_ * 2).runAsync(_ mustBe 84)
    }

    "suspend computation" in {
      var suspended = true
      val io = IO.delay { suspended = false }

      suspended mustBe true
      io.runAsync(_ => ())
      suspended mustBe false
    }

    "flat map values" in {
      val io = IO
        .delay(println("starting io chain"))
        .as(42)
        .flatMap(a => IO.delay(println("doing computation")).as(2).flatMap(b => IO.delay(a * b)))

      io.runAsync(_ mustBe 84)
    }
  }
}
