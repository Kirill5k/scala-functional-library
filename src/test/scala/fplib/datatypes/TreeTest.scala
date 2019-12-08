package fplib.datatypes

import org.scalatest.{FunSpec, Matchers}

class TreeTest extends FunSpec with Matchers {

  val ts = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
  val ti = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  describe("size") {
    it("should count all leafs and branches") {
      Tree.size(ts) should be (7)
    }
  }

  describe("maximum") {
    it("should return max of ints tree") {
      Tree.maximum(ti) should be (4)
    }
  }

  describe("depth") {
    it("should return depth of a tree") {
      Tree.depth(ti) should be (3)
    }
  }

  describe("map") {
    it("should map all values in a tree") {
      Tree.map(ts)(_ * 2) should be (Branch(Branch(Leaf("aa"), Leaf("bb")), Branch(Leaf("cc"), Leaf("dd"))))
    }
    it("should map all values in a tree via fold") {
      Tree.mapViaFold(ts)(_ * 2) should be (Branch(Branch(Leaf("aa"), Leaf("bb")), Branch(Leaf("cc"), Leaf("dd"))))
    }
  }

  describe("flatMap") {
    it("should map all values in a tree") {
      Tree.flatMap(ts)(x => Leaf(x * 2)) should be (Branch(Branch(Leaf("aa"), Leaf("bb")), Branch(Leaf("cc"), Leaf("dd"))))
    }
  }

  describe("fold") {
    it("should fold all els to 1") {
      Tree.fold(ts)(a => a)(_+_) should be ("abcd")
    }
  }
}
