package fplib.utils

import fplib.effects.Monoid

sealed trait WordCount
case class Stub(chars: String) extends WordCount
case class Part(lStub: String, words: Int, rStub: String) extends WordCount

object WordCount {

  // convert to indexed sex
  // map each char to stub
  // apply monoid

  def count(s: String): Int = ???
}
