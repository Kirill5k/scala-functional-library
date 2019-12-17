package fplib.effects

import org.scalatest.{FunSpec, Matchers}

class MonoidTest extends FunSpec with Matchers {

  describe("Monoid") {
    describe("listMonoid") {
      it("should concat 2 lists together") {
        val result = Monoid.listMonoid.op(List(1,2,3), List(4,5))
        result should be (List(1,2,3,4,5))
      }
    }
  }
}
