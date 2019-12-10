package fplib.datatypes

import org.scalatest.{FunSpec, Matchers}

class StreamTest extends FunSpec with Matchers {

  describe("stream") {
    val ints = Stream(1,2,3,4,5)
    val ints2 = Stream(6,7,8,9)
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
        ints.takeWhile(i => i < 2 || i > 4).toList should be (List(1))
        ints.takeWhile(_ < 4).toList should be (List(1,2,3))
        ints.takeWhile(_ > 0).toList should be (List(1,2,3,4,5))
        ints.takeWhileViaFold(i => i < 2 || i > 4).toList should be (List(1))
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

    describe("map") {
      it("should apply f to all elements") {
        ints.map(_.toString).toList should be (List("1", "2", "3", "4", "5"))
        ints.mapViaFold(_.toString).toList should be (List("1", "2", "3", "4", "5"))
      }
    }

    describe("append") {
      it("should append stream") {
        ints.append(ints2).toList should be (List(1,2,3,4,5,6,7,8,9))
        ints.appendViaFold(ints2).toList should be (List(1,2,3,4,5,6,7,8,9))
      }
    }

    describe("filter") {
      it("should filter elements based on predicate") {
        ints.filter(i => i < 2 || i > 4).toList should be (List(1,5))
        ints.filter(_ % 2 == 0).toList should be (List(2,4))
        ints.filterViaFold(_ % 2 == 0).toList should be (List(2,4))
        ints.filterViaFold(i => i < 2 || i > 4).toList should be (List(1,5))
      }
    }

    describe("flamtMap") {
      it("should apply f to all elements") {
        ints.map(i => Stream(i.toString)).toList should be (List("1", "2", "3", "4", "5"))
      }
    }
  }
}
