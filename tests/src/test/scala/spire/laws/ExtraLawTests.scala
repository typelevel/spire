package spire.laws

import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import spire.math.extras.interval.{IntervalTrie, IntervalTrieArbitrary, IntervalSeq, IntervalSeqArbitrary}
import spire.implicits._

class ExtraLawTests extends FunSuite with Discipline {
  import IntervalTrieArbitrary._
  import IntervalSeqArbitrary._
  checkAll("Bool[IntervalSeq[Int]]", LogicLaws[IntervalSeq[Int]].bool)
  checkAll("Bool[IntervalTrie[Long]]", LogicLaws[IntervalTrie[Long]].bool)
}
