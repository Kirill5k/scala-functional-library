package fplib.effects

import org.scalatest.{FunSpec, Matchers}

class RngTest extends FunSpec with Matchers {

  describe("SimpleRng") {
    it("should generate random int and return state") {
      val rng = SimpleRng(42)
      val (n1, rng2) = rng.nextInt
      n1 should be (16159453)
      rng2 should be (SimpleRng(1059025964525L))
    }

    it("should generate list of random ints") {
      val rng = SimpleRng(42)
      val (ints, rng2) = Rng.ints(5)(rng)
      ints should have size 5
    }
  }

}
