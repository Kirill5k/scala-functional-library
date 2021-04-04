package fplib.data

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListTest extends AnyWordSpec with Matchers {

  "A List" when {
    val ints = List(1,2,3,4,5)
    val int2 = List(6,7,8,9)
    val doubles = List(1.0, 2.0, 3.0, 4.0, 5.0)
    val strings = List("a", "b", "c", "d", "e")

    "init" should {
      "remove the last element from list" in {
        List.init(ints) mustBe (List(1, 2, 3, 4))
      }
    }

    "product" should {
      "multiply numbers" in {
        List.product(doubles) mustBe (120)
      }
      "multiply numbers via fold" in {
        List.productViaFold(doubles) mustBe (120)
      }
    }

    "append" should {
      "append a list" in {
        List.append(ints, int2) mustBe (List(1,2,3,4,5,6,7,8,9))
      }
      "append a list via fold" in {
        List.appendViaFold(ints)(int2) mustBe (List(1,2,3,4,5,6,7,8,9))
      }
    }

    "foldLeft" should {
      "reduce all to 1" in {
        List.foldLeft(ints)(0)(_+_) mustBe (15)
      }

      "concat strings" in {
        List.foldLeft(strings)("")(_ + _) mustBe ("abcde")
      }
    }

    "foldRight" should {
      "reduce all to 1" in {
        List.foldRight(ints)(0)(_+_) mustBe (15)
      }

      "concat strings" in {
        List.foldRight(strings)("")(_ + _) mustBe ("abcde")
      }
    }

    "length" should {
      "return length of a list" in {
        val list = List(1,2,3,4,5)
        List.length(list) mustBe (5)
      }

      "return length of an empty list" in {
        List.length(EmptyList) mustBe (0)
      }
    }

    "reverse" should {
      "reverse a list" in {
        List.reverse(ints) mustBe (List(5,4,3,2,1))
      }
    }

    "foldLeftViaFoldRight" should {
      "concat starting from head" in {
        List.foldLeftViaFoldRight(strings)("")(_+_) mustBe ("abcde")
      }
    }

    "concat" should {
      "flatten list of lists into list" in {
        List.flatten(List(ints, int2)) mustBe (List(1,2,3,4,5,6,7,8,9))
      }
    }

    "addOne" should {
      "add 1 to all els" in {
        List.addOne(ints) mustBe (List(2,3,4,5,6))
      }

      "add 1 to all els via map" in {
        List.addOneViaMap(ints) mustBe (List(2,3,4,5,6))
      }
    }

    "map" should {
      "apply f to all els" in {
        List.map(ints)(_ * 10) mustBe (List(10, 20, 30, 40, 50))
      }
    }

    "filter" should {
      "filter elements based on predicate" in {
        List.filter(ints)(_ % 2 == 0) mustBe (List(2, 4))
      }

      "filter elements based on predicate via fold" in {
        List.filterViaFold(ints)(_ % 2 == 0) mustBe (List(2, 4))
      }

      "filter elements based on predicate via flatmap" in {
        List.filterViaFlatMap(ints)(_ % 2 == 0) mustBe (List(2, 4))
      }
    }

    "flatMap" should {
      "apply f to all els" in {
        List.flatMap(ints)(a => List((a*10).toString)) mustBe (List("10", "20", "30", "40", "50"))
      }
    }

    "zipWith" should {
      "zip 2 lists and apply f" in {
        List.zipWith(ints, doubles)(_ * _) mustBe (List(1.0, 4.0, 9.0, 16.0, 25.0))
      }

      "return empty list if one of lists is empty" in {
        List.zipWith(ints, List[Int]())(_ * _) mustBe (List())
        List.zipWith(List[Double](), doubles)(_ * _) mustBe (List())
      }
    }
  }
}
