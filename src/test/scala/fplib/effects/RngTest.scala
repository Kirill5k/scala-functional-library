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

    it("should generate random double and return state") {
      val rng = SimpleRng(42)
      val (n1, rng2) = Rng.double(rng)
      n1 should be (0.007524831686168909)
      rng2 should be (SimpleRng(1059025964525L))
    }

    it("should generate list of random ints") {
      val rng = SimpleRng(42)
      val (ints1, _) = Rng.ints(5)(rng)
      ints1 should be (List(16159453, -1281479697, -340305902, -2015756020, 1770001318))
      val (ints2, _) = Rng.intsViaSequence(5)(rng)
      ints2 should be (List(16159453, -1281479697, -340305902, -2015756020, 1770001318))
    }

    it("should generate a pair of int and double") {
      val rng = SimpleRng(42)
      val ((i1, d1), rng1) = Rng.intDouble(rng)
      val ((i2, d2), rng2) = Rng.intDoubleViaMap2(rng)
      ((i1, d1), rng1) should be ((i2, d2), rng2)
    }
  }

}
