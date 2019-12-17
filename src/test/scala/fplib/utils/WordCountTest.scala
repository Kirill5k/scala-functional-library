package fplib.utils

import org.scalatest.{FunSpec, Matchers}

class WordCountTest extends FunSpec with Matchers {

  describe("WordCount") {
    it("should count words in sentence") {
      val text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit"
      WordCount.count(text) should be (8)
    }

    it("should count words in sentence consisting of 1 word") {
      WordCount.count("Lorem") should be (1)
      WordCount.count(",") should be (1)
      WordCount.count(" ") should be (0)
    }
  }

}
