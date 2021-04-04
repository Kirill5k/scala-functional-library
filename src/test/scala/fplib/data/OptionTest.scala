package fplib.data

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.List

class OptionTest extends AnyWordSpec with Matchers {

  "An Option" should {
    "map" in {
      Some("a").map(_ * 2) mustBe (Some("aa"))
      None.map(_.toString) mustBe (None)
    }

    "filter" in {
      Some(1).filter(_ > 2) mustBe (None)
      Some(1).filter(_ == 1) mustBe (Some(1))
    }

    "flatMap" in {
      Some(1).flatMap(a => Some(a * 2)) mustBe (Some(2))
      Some(1).flatMap(_ => None) mustBe (None)
      None.flatMap(a => Some(a.toString)) mustBe (None)
    }

    "orElse" in {
      Some(1).orElse(Some(2)) mustBe (Some(1))
      None.orElse(Some(2)) mustBe (Some(2))
    }

    "getOrElse" in {
      Some(1).getOrElse(2) mustBe (1)
      None.getOrElse(2) mustBe (2)
    }

    "map2" in {
      Option.map2(Some(1), Some(2))(_ + _) mustBe (Some(3))
      Option.map2(None: Option[Int], Some(2))(_ + _) mustBe (None)
      Option.map2(Some(1),None)(_ + _) mustBe (None)
    }

    "sequence" in {
      Option.sequence(List(Some(1), Some(2), Some(3))) mustBe (Some(List(1,2,3)))
      Option.sequence(List(Some(1), None, Some(3))) mustBe (None)
    }

    "traverse" in {
      Option.traverse(List(1, 2, 3))(a => Some(a.toString)) mustBe (Some(List("1","2","3")))
    }
  }
}
