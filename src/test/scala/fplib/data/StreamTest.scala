package fplib.data

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StreamTest extends AnyWordSpec with Matchers {

  "A Stream" when {
    val ints = Stream(1,2,3,4,5)
    val ints2 = Stream(6,7,8,9)
    val strings = Stream("a", "b", "c", "d", "e")

    "head" should {
      "return head of non empty stream" in {
        ints.head mustBe (Some(1))
        ints.headViaFold mustBe (Some(1))
      }
      "return none of empty stream" in {
        Stream().head mustBe (None)
      }
    }

    "toList" should {
      "convert stream to list" in {
        ints.toList mustBe (List(1,2,3,4,5))
        Stream.empty.toList mustBe (EmptyList)
      }
    }

    "take" should {
      "return first n elements" in {
        ints.take(3).toList mustBe (List(1,2,3))
        ints.takeViaUnfold(3).toList mustBe (List(1,2,3))
      }
    }

    "takeWhile" should {
      "return elements while function is true" in {
        ints.takeWhile(i => i < 2 || i > 4).toList mustBe (List(1))
        ints.takeWhile(_ < 4).toList mustBe (List(1,2,3))
        ints.takeWhile(_ > 0).toList mustBe (List(1,2,3,4,5))
        ints.takeWhileViaFold(i => i < 2 || i > 4).toList mustBe (List(1))
        ints.takeWhileViaFold(_ < 4).toList mustBe (List(1,2,3))
        ints.takeWhileViaFold(_ > 0).toList mustBe (List(1,2,3,4,5))
        ints.takeWhileViaUnfold(i => i < 2 || i > 4).toList mustBe (List(1))
        ints.takeWhileViaUnfold(_ < 4).toList mustBe (List(1,2,3))
        ints.takeWhileViaUnfold(_ > 0).toList mustBe (List(1,2,3,4,5))
      }
    }

    "exists" should {
      "return true if condition matches" in {
        ints.exists(_ > 4) mustBe (true)
        ints.existsViaFold(_ > 4) mustBe (true)
        ints.exists(_ > 10) mustBe (false)
        ints.existsViaFold(_ > 10) mustBe (false)
      }
    }

    "forAll" should {
      "return true if condition matches for all elements" in {
        ints.forAll(_ > 0) mustBe (true)
        ints.forAll(_ > 4) mustBe (false)
      }
    }

    "foldRight" should {
      "return true if condition matches for all elements" in {
        ints.foldRight(0)(_+_) mustBe (15)
        strings.foldRight("")(_+_) mustBe ("abcde")
      }
    }

    "map" should {
      "apply f to all elements" in {
        ints.map(_.toString).toList mustBe (List("1", "2", "3", "4", "5"))
        ints.mapViaFold(_.toString).toList mustBe (List("1", "2", "3", "4", "5"))
        ints.mapViaUnfold(_.toString).toList mustBe (List("1", "2", "3", "4", "5"))
      }
    }

    "append" should {
      "append stream" in {
        ints.append(ints2).toList mustBe (List(1,2,3,4,5,6,7,8,9))
        ints.appendViaFold(ints2).toList mustBe (List(1,2,3,4,5,6,7,8,9))
      }
    }

    "filter" should {
      "filter elements based on predicate" in {
        ints.filter(i => i < 2 || i > 4).toList mustBe (List(1,5))
        ints.filter(_ % 2 == 0).toList mustBe (List(2,4))
        ints.filterViaFold(_ % 2 == 0).toList mustBe (List(2,4))
        ints.filterViaFold(i => i < 2 || i > 4).toList mustBe (List(1,5))
      }
    }

    "flatMap" should {
      "apply f to all elements" in {
        ints.flatMap(i => Stream(i.toString)).toList mustBe (List("1", "2", "3", "4", "5"))
      }
    }

    "find" should {
      "find an element based on predicate" in {
        ints.find(_ > 3) mustBe (Some(4))
        ints.find(_ > 10) mustBe (None)
      }
    }

    "constant" should {
      "create infinite stream" in {
        Stream.constant(1).take(10).toList mustBe (List(1,1,1,1,1,1,1,1,1,1))
        Stream.constantViaUnfold(1).take(10).toList mustBe (List(1,1,1,1,1,1,1,1,1,1))
      }
    }

    "from" should {
      "create infinite stream of number" in {
        Stream.from(1).take(10).toList mustBe (List(1,2,3,4,5,6,7,8,9,10))
        Stream.fromViaUnfold(1).take(10).toList mustBe (List(1,2,3,4,5,6,7,8,9,10))
      }
    }

    "fibs" should {
      "generate sequence of fibs" in {
        Stream.fibs().take(10).toList mustBe (List(0,1,1,2,3,5,8,13,21,34))
        Stream.fibsViaUnfold().take(10).toList mustBe (List(0,1,1,2,3,5,8,13,21,34))
      }
    }

    "zipAll" should {
      "zip 2 streams together" in {
        Stream.constant(1).take(5).zipAll(Stream.constant(2).take(4)).toList mustBe
        (List((Some(1), Some(2)), (Some(1), Some(2)), (Some(1), Some(2)), (Some(1), Some(2)), (Some(1), None)))
      }
    }

    "tails" should {
      "split stream into subsequences" in {
        ints.tails.map(_.toList).toList mustBe (List(List(1,2,3,4,5), List(2,3,4,5), List(3,4,5),  List(4,5), List(5), List()))
      }
    }

    "startWith" should {
      "split stream into subsequences" in {
        ints.startsWith(Stream(1,2,3)) mustBe (true)
        ints.startsWith(Stream(2,3)) mustBe (false)
        ints.startsWith(Stream(1,2,3,4,5,6)) mustBe (false)
      }
    }

    "scanRight" should {
      "return stream of intermediate results" in {
        ints.scanRight(0)(_+_).toList mustBe (List(15, 14, 12, 9, 5,0))
      }
    }
  }
}
