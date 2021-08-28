package spire
package math

import spire.implicits.{eqOps => _, _}

class IntervalSubsetPartialOrderSuite extends munit.FunSuite {
  import spire.optional.intervalSubsetPartialOrder._

  import Interval.{closed, point}

  test("Minimal and maximal elements of {[1, 3], [3], [2], [1]} by subset partial order") {
    val intervals = Seq(closed(1, 3), point(3), point(2), point(1))
    assertEquals(seqOps(intervals).pmin.toSet, Set(point(1), point(2), point(3)))
    assertEquals(seqOps(intervals).pmax.toSet, Set(closed(1, 3)))
  }
}
