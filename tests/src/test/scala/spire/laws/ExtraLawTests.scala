package spire.laws

import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import spire.math.extras.interval.{IntervalSeq, IntervalSeqArbitrary} // IntervalTrie, IntervalTrieArbitrary
import spire.implicits._

class ExtraLawTests extends FunSuite with Discipline {
  import IntervalSeqArbitrary._
  // import IntervalTrieArbitrary._
  checkAll("Bool[IntervalSeq[Int]]", LogicLaws[IntervalSeq[Int]].bool)
//  checkAll("Bool[IntervalTrie[Long]]", LogicLaws[IntervalTrie[Long]].bool)
}
