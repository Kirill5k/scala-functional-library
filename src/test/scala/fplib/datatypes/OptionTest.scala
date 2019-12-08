package fplib.datatypes

import org.scalatest.{FunSpec, Matchers}

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
  }
}
