package spire.laws

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import spire.math.extras.interval.{IntervalSeq, IntervalSeqArbitrary} // IntervalTrie, IntervalTrieArbitrary
import spire.implicits._

class ExtraLawTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  import IntervalSeqArbitrary._
  // import IntervalTrieArbitrary._

  checkAll("Bool[IntervalSeq[Int]]", LogicLaws[IntervalSeq[Int]].bool)
  // TODO: restore the test
//  checkAll("Bool[IntervalTrie[Long]]", LogicLaws[IntervalTrie[Long]].bool)
}
