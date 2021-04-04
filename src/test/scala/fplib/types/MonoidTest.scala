package fplib.types

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MonoidTest extends AnyWordSpec with Matchers {

  "A Monoid" when {
    "listMonoid" should {
      "concat 2 lists together" in {
        val result = Monoid.listMonoid.op(List(1, 2, 3), List(4, 5))
        result mustBe (List(1, 2, 3, 4, 5))
      }
    }

    "concatenate" should {
      "concat all list elements together" in {
        val result = Monoid.concatenate(List(1, 2, 3, 4, 5, 6))(Monoid.intAdditionMonoid)
        result mustBe 21
      }
    }

    "foldMap" should {
      "map and concat all List elements together" in {
        val result = Monoid.foldMap[Int, String](List(1, 2, 3, 4, 5, 6))(_.toString)(Monoid.stringMonoid)
        result mustBe "123456"
      }

      "map and concat all IndexedSeq elements together" in {
        val result = Monoid.foldMap[Int, String](IndexedSeq(1, 2, 3, 4, 5, 6))(_.toString)(Monoid.stringMonoid)
        result mustBe "123456"
      }
    }
  }
}
