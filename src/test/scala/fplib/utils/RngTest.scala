package fplib.utils

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec


class RngTest extends AnyWordSpec with Matchers {

  "A SimpleRng" should {
    "generate random int and return state" in {
      val rng = SimpleRng(42)
      val (n1, rng2) = rng.nextInt
      n1 mustBe (16159453)
      rng2 mustBe (SimpleRng(1059025964525L))
    }

    "generate random double and return state" in {
      val rng = SimpleRng(42)
      val (n1, rng2) = Rng.double(rng)
      n1 mustBe (0.007524831686168909)
      rng2 mustBe (SimpleRng(1059025964525L))
    }

    "generate list of random ints" in {
      val rng = SimpleRng(42)
      val (ints1, _) = Rng.ints(5)(rng)
      ints1 mustBe (List(16159453, -1281479697, -340305902, -2015756020, 1770001318))
      val (ints2, _) = Rng.intsViaSequence(5)(rng)
      ints2 mustBe (List(16159453, -1281479697, -340305902, -2015756020, 1770001318))
    }

    "generate a pair of int and double" in {
      val rng = SimpleRng(42)
      val ((i1, d1), rng1) = Rng.intDouble(rng)
      val ((i2, d2), rng2) = Rng.intDoubleViaMap2(rng)
      ((i1, d1), rng1) mustBe ((i2, d2), rng2)
    }
  }

}
