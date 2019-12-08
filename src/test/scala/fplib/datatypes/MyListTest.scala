package fplib.datatypes

import org.scalatest.{FunSpec, Matchers}

class MyListTest extends FunSpec with Matchers {

  describe("list") {
    val ints = MyList(1,2,3,4,5)
    val int2 = MyList(6,7,8,9)
    val doubles = MyList(1.0, 2.0, 3.0, 4.0, 5.0)
    val strings = MyList("a", "b", "c", "d", "e")

    describe("init") {
      it("should remove the last element from list") {
        MyList.init(ints) should be (MyList(1, 2, 3, 4))
      }
    }

    describe("product") {
      it("should multiply numbers") {
        MyList.product(doubles) should be (120)
      }
      it("should multiply numbers via fold") {
        MyList.productViaFold(doubles) should be (120)
      }
    }

    describe("append") {
      it("should append a list") {
        MyList.append(ints, int2) should be (MyList(1,2,3,4,5,6,7,8,9))
      }
      it("should append a list via fold") {
        MyList.appendViaFold(ints)(int2) should be (MyList(1,2,3,4,5,6,7,8,9))
      }
    }

    describe("foldLeft") {
      it("should reduce all to 1") {
        MyList.foldLeft(ints)(0)(_+_) should be (15)
      }

      it("should concat strings") {
        MyList.foldLeft(strings)("")(_ + _) should be ("abcde")
      }
    }

    describe("foldRight") {
      it("should reduce all to 1") {
        MyList.foldRight(ints)(0)(_+_) should be (15)
      }

      it("should concat strings") {
        MyList.foldRight(strings)("")(_ + _) should be ("abcde")
      }
    }

    describe("length") {
      it("should return length of a list") {
        val list = MyList(1,2,3,4,5)
        MyList.length(list) should be (5)
      }

      it("should return length of an empty list") {
        MyList.length(Nil) should be (0)
      }
    }

    describe("reverse") {
      it("should reverse a list") {
        MyList.reverse(ints) should be (MyList(5,4,3,2,1))
      }
    }

    describe("foldLeftViaFoldRight") {
      it("should concat starting from head") {
        MyList.foldLeftViaFoldRight(strings)("")(_+_) should be ("abcde")
      }
    }

    describe("concat") {
      it("should flatten list of lists into list") {
        MyList.flatten(MyList(ints, int2)) should be (MyList(1,2,3,4,5,6,7,8,9))
      }
    }

    describe("addOne") {
      it("should add 1 to all els") {
        MyList.addOne(ints) should be (MyList(2,3,4,5,6))
      }

      it("should add 1 to all els via map") {
        MyList.addOneViaMap(ints) should be (MyList(2,3,4,5,6))
      }
    }

    describe("map") {
      it("should apply f to all els") {
        MyList.map(ints)(_ * 10) should be (MyList(10, 20, 30, 40, 50))
      }
    }

    describe("filter") {
      it("should filter elements based on predicate") {
        MyList.filter(ints)(_ % 2 == 0) should be (MyList(2, 4))
      }

      it("should filter elements based on predicate via fold") {
        MyList.filterViaFold(ints)(_ % 2 == 0) should be (MyList(2, 4))
      }

      it("should filter elements based on predicate via flatmap") {
        MyList.filterViaFlatMap(ints)(_ % 2 == 0) should be (MyList(2, 4))
      }
    }

    describe("flatMap") {
      it("should apply f to all els") {
        MyList.flatMap(ints)(a => MyList((a*10).toString)) should be (MyList("10", "20", "30", "40", "50"))
      }
    }

    describe("zipWith") {
      it("should zip 2 lists and apply f") {
        MyList.zipWith(ints, doubles)(_ * _) should be (MyList(1.0, 4.0, 9.0, 16.0, 25.0))
      }

      it("should return empty list if one of lists is empty") {
        MyList.zipWith(ints, MyList[Int]())(_ * _) should be (MyList())
        MyList.zipWith(MyList[Double](), doubles)(_ * _) should be (MyList())
      }
    }
  }
}
