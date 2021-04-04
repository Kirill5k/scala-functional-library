package fplib.effects

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IOSpec extends AnyWordSpec with Matchers {

  "An IO" should {

    "return pure value" in {
      IORunner.run(IO.pure(42)) mustBe 42
    }

    "zip 2 values together" in {
      IORunner.run(IO.pure(42).zip(IO.pure("42"))) mustBe (42, "42")
    }

    "map value inside" in {
      IORunner.run(IO.pure(42).map(_ * 2)) mustBe 84
    }

    "suspend computation" in {
      var suspended = true
      val io = IO.delay { suspended = false }

      suspended mustBe true
      IORunner.run(io)
      suspended mustBe false
    }

    "flat map values" in {
      val io = IO
        .delay(println("starting io chain"))
        .as(42)
        .flatMap(a => IO.delay(println("doing computation")).as(2).flatMap(b => IO.delay(a * b)))

      IORunner.run(io) mustBe 84
    }
  }
}
