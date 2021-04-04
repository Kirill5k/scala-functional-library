package fplib.utils

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class WordCountTest extends AnyWordSpec with Matchers {

  "A WordCount" should {
    "count words in sentence" in {
      val text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit"
      WordCount.count(text) mustBe (8)
    }

    "count words in sentence consisting of 1 word" in {
      WordCount.count("Lorem") mustBe (1)
      WordCount.count(",") mustBe (1)
      WordCount.count(" ") mustBe (0)
    }
  }

}
