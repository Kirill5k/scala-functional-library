package fplib.utils

import fplib.effects.Monoid

sealed trait WordCount
case class Stub(chars: String) extends WordCount
case class Part(lStub: String, words: Int, rStub: String) extends WordCount

object WordCount {
  def count(s: String): Int = {
    val charToStub: Char => WordCount = c => if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    val unstub: String => Int = x => x.length.min(1)

    Monoid.foldMap(s.toIndexedSeq)(charToStub)(Monoid.wordCountMonoid) match {
      case Stub(x) => unstub(x)
      case Part(l, c, r) =>unstub(l) + c + unstub(r)
    }
  }
}
