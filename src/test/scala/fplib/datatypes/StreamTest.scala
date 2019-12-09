package fplib.datatypes

import org.scalatest.{FunSpec, Matchers}

class StreamTest extends FunSpec with Matchers {

  describe("stream") {
    val ints = Stream(1,2,3,4,5)
    val strings = Stream("a", "b", "c", "d", "e")

    describe("head") {
      it("returns head of non empty stream") {
        ints.head should be (Some(1))
        ints.headViaFold should be (Some(1))
      }
      it("returns none of empty stream") {
        Stream().head should be (None)
      }
    }

    describe("toList") {
      it("converts stream to list") {
        ints.toList should be (List(1,2,3,4,5))
        Stream.empty.toList should be (EmptyList)
      }
    }

    describe("take") {
      it("should return first n elements") {
        ints.take(3).toList should be (List(1,2,3))
      }
    }

    describe("take while") {
      it("should return elements while function is true") {
        ints.takeWhile(_ < 4).toList should be (List(1,2,3))
        ints.takeWhile(_ > 0).toList should be (List(1,2,3,4,5))
        ints.takeWhileViaFold(_ < 4).toList should be (List(1,2,3))
        ints.takeWhileViaFold(_ > 0).toList should be (List(1,2,3,4,5))
      }
    }

    describe("exists") {
      it("should return true if condition matches") {
        ints.exists(_ > 4) should be (true)
        ints.existsViaFold(_ > 4) should be (true)
        ints.exists(_ > 10) should be (false)
        ints.existsViaFold(_ > 10) should be (false)
      }
    }

    describe("forAll") {
      it("should return true if condition matches for all elements") {
        ints.forAll(_ > 0) should be (true)
        ints.forAll(_ > 4) should be (false)
      }
    }

    describe("foldRight") {
      it("should return true if condition matches for all elements") {
        ints.foldRight(0)(_+_) should be (15)
        strings.foldRight("")(_+_) should be ("abcde")
      }
    }
  }
}
