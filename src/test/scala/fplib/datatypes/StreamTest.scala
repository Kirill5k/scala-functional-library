package fplib.datatypes

import org.scalatest.{FunSpec, Matchers}

class StreamTest extends FunSpec with Matchers {

  describe("stream") {
    val ints = Stream(1,2,3,4,5)

    describe("head") {
      it("returns head of non empty stream") {
        ints.head should be (Some(1))
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
      }
    }
  }
}
