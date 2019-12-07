package fplib.datatypes

import org.scalatest.{FunSpec, Matchers}

class ListTest extends FunSpec with Matchers {

  describe("list") {
    describe("init") {
      it("should remove the last element from list") {
        val list = List.apply(1, 2, 3, 4)
        List.init(list) should be (List(1, 2, 3))
      }
    }

    describe("foldLeft") {
      it("should reduce all to 1") {
        val list = List(1,2,3,4,5)
        List.foldLeft(list)(0)(_+_) should be (15)
      }
    }

    describe("foldRight") {
      it("should reduce all to 1") {
        val list = List(1,2,3,4,5)
        List.foldRight(list)(0)(_+_) should be (15)
      }
    }
  }
}
