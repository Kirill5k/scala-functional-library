package fplib.effects

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IOSpec extends AnyWordSpec with Matchers {

  "An IO" should {

    "return pure value" in {

      IORunner.run(IO.pure(42)) mustBe 42
    }
  }
}
