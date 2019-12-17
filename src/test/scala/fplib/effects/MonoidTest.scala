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

    describe("concatenate") {
      it("should concat all list elements together") {
        val result = Monoid.concatenate(List(1,2,3,4,5,6))(Monoid.intAdditionMonoid)
        result should be (21)
      }
    }

    describe("foldMap") {
      it("should map and concat all list elements together") {
        val result = Monoid.foldMap(List(1,2,3,4,5,6))(_.toString)(Monoid.stringMonoid)
        result should be ("123456")
      }
    }
  }
}
