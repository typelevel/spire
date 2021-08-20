package spire.laws

import spire.math.extras.interval.{IntervalSeq, IntervalSeqArbitrary, IntervalTrie}
import spire.math.extras.interval.IntervalTrieArbitrary._
import spire.implicits._

class ExtraLawSuite extends munit.DisciplineSuite {
  // import IntervalSeqArbitrary._
  //
  // checkAll("Bool[IntervalSeq[Int]]", LogicLaws[IntervalSeq[Int]].bool)
  // checkAll("Bool[IntervalTrie[Long]]", LogicLaws[IntervalTrie[Long]].bool)
}
