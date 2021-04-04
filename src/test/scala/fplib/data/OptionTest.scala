package fplib.data

import org.scalatest.{FunSpec, Matchers}
import scala.collection.immutable.List

class OptionTest extends FunSpec with Matchers {

  describe("option") {
    it("map") {
      Some("a").map(_ * 2) should be (Some("aa"))
      None.map(_.toString) should be (None)
    }

    it("filter") {
      Some(1).filter(_ > 2) should be (None)
      Some(1).filter(_ == 1) should be (Some(1))
    }

    it("flatMap") {
      Some(1).flatMap(a => Some(a * 2)) should be (Some(2))
      Some(1).flatMap(_ => None) should be (None)
      None.flatMap(a => Some(a.toString)) should be (None)
    }

    it("orElse") {
      Some(1).orElse(Some(2)) should be (Some(1))
      None.orElse(Some(2)) should be (Some(2))
    }

    it("getOrElse") {
      Some(1).getOrElse(2) should be (1)
      None.getOrElse(2) should be (2)
    }

    it("map2") {
      Option.map2(Some(1), Some(2))(_ + _) should be (Some(3))
      Option.map2(None: Option[Int], Some(2))(_ + _) should be (None)
      Option.map2(Some(1),None)(_ + _) should be (None)
    }

    it("sequence") {
      Option.sequence(List(Some(1), Some(2), Some(3))) should be (Some(List(1,2,3)))
      Option.sequence(List(Some(1), None, Some(3))) should be (None)
    }

    it("traverse") {
      Option.traverse(List(1, 2, 3))(a => Some(a.toString)) should be (Some(List("1","2","3")))
    }
  }
}
