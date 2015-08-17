package spire.laws

import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import spire.math.interval.IntervalSet

class ExtraLawTests extends FunSuite with Discipline {
  import spire.std.long.LongAlgebra
  import spire.math.interval.IntervalSetAlgebra._
  import spire.math.interval.IntervalSetArbitrary._
  checkAll("Bool[IntervalSet[Long]]", LogicLaws[IntervalSet[Long]].bool)
}
