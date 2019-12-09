package fplib.datatypes

import org.scalatest.{FunSpec, Matchers}

class ListTest extends FunSpec with Matchers {

  describe("list") {
    val ints = List(1,2,3,4,5)
    val int2 = List(6,7,8,9)
    val doubles = List(1.0, 2.0, 3.0, 4.0, 5.0)
    val strings = List("a", "b", "c", "d", "e")

    describe("init") {
      it("should remove the last element from list") {
        List.init(ints) should be (List(1, 2, 3, 4))
      }
    }

    describe("product") {
      it("should multiply numbers") {
        List.product(doubles) should be (120)
      }
      it("should multiply numbers via fold") {
        List.productViaFold(doubles) should be (120)
      }
    }

    describe("append") {
      it("should append a list") {
        List.append(ints, int2) should be (List(1,2,3,4,5,6,7,8,9))
      }
      it("should append a list via fold") {
        List.appendViaFold(ints)(int2) should be (List(1,2,3,4,5,6,7,8,9))
      }
    }

    describe("foldLeft") {
      it("should reduce all to 1") {
        List.foldLeft(ints)(0)(_+_) should be (15)
      }

      it("should concat strings") {
        List.foldLeft(strings)("")(_ + _) should be ("abcde")
      }
    }

    describe("foldRight") {
      it("should reduce all to 1") {
        List.foldRight(ints)(0)(_+_) should be (15)
      }

      it("should concat strings") {
        List.foldRight(strings)("")(_ + _) should be ("abcde")
      }
    }

    describe("length") {
      it("should return length of a list") {
        val list = List(1,2,3,4,5)
        List.length(list) should be (5)
      }

      it("should return length of an empty list") {
        List.length(EmptyList) should be (0)
      }
    }

    describe("reverse") {
      it("should reverse a list") {
        List.reverse(ints) should be (List(5,4,3,2,1))
      }
    }

    describe("foldLeftViaFoldRight") {
      it("should concat starting from head") {
        List.foldLeftViaFoldRight(strings)("")(_+_) should be ("abcde")
      }
    }

    describe("concat") {
      it("should flatten list of lists into list") {
        List.flatten(List(ints, int2)) should be (List(1,2,3,4,5,6,7,8,9))
      }
    }

    describe("addOne") {
      it("should add 1 to all els") {
        List.addOne(ints) should be (List(2,3,4,5,6))
      }

      it("should add 1 to all els via map") {
        List.addOneViaMap(ints) should be (List(2,3,4,5,6))
      }
    }

    describe("map") {
      it("should apply f to all els") {
        List.map(ints)(_ * 10) should be (List(10, 20, 30, 40, 50))
      }
    }

    describe("filter") {
      it("should filter elements based on predicate") {
        List.filter(ints)(_ % 2 == 0) should be (List(2, 4))
      }

      it("should filter elements based on predicate via fold") {
        List.filterViaFold(ints)(_ % 2 == 0) should be (List(2, 4))
      }

      it("should filter elements based on predicate via flatmap") {
        List.filterViaFlatMap(ints)(_ % 2 == 0) should be (List(2, 4))
      }
    }

    describe("flatMap") {
      it("should apply f to all els") {
        List.flatMap(ints)(a => List((a*10).toString)) should be (List("10", "20", "30", "40", "50"))
      }
    }

    describe("zipWith") {
      it("should zip 2 lists and apply f") {
        List.zipWith(ints, doubles)(_ * _) should be (List(1.0, 4.0, 9.0, 16.0, 25.0))
      }

      it("should return empty list if one of lists is empty") {
        List.zipWith(ints, List[Int]())(_ * _) should be (List())
        List.zipWith(List[Double](), doubles)(_ * _) should be (List())
      }
    }
  }
}
