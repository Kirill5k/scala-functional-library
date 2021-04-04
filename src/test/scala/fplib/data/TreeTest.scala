package fplib.data

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TreeTest extends AnyWordSpec with Matchers {

  val ts = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
  val ti = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  "size" should {
    "should count all leafs and branches" in {
      Tree.size(ts) mustBe (7)
    }
  }

  "maximum" should {
    "should return max of ints tree" in {
      Tree.maximum(ti) mustBe (4)
    }
  }

  "depth" should {
    "should return depth of a tree" in {
      Tree.depth(ti) mustBe (3)
    }
  }

  "map" should {
    "should map all values in a tree" in {
      Tree.map(ts)(_ * 2) mustBe (Branch(Branch(Leaf("aa"), Leaf("bb")), Branch(Leaf("cc"), Leaf("dd"))))
    }
    "should map all values in a tree via fold" in {
      Tree.mapViaFold(ts)(_ * 2) mustBe (Branch(Branch(Leaf("aa"), Leaf("bb")), Branch(Leaf("cc"), Leaf("dd"))))
    }
  }

  "flatMap" should {
    "should map all values in a tree" in {
      Tree.flatMap(ts)(x => Leaf(x * 2)) mustBe (Branch(Branch(Leaf("aa"), Leaf("bb")), Branch(Leaf("cc"), Leaf("dd"))))
    }
  }

  "fold" should {
    "should fold all els to 1" in {
      Tree.fold(ts)(a => a)(_+_) mustBe ("abcd")
    }
  }
}
