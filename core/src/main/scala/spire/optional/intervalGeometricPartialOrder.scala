package spire
package optional

import spire.algebra._
import spire.math.Interval
import spire.syntax.order._

object intervalGeometricPartialOrder {
  import spire.math.interval._

  /**
   * Interval partial order defined as follows:
   *
   * Involving empty intervals:
   *
   *   - if I and J are empty, then I === J.
   *   - if I (resp. J) is empty and J (resp. I) is non-empty, the ordering is undefined (preserving antisymmetry).
   *
   * For non-empty intervals:
   *
   *   - I === J is standard Eq semantics (I, J are intersubstituable)
   *   - I < J if all x \in I, y \in J have x < y
   *   - I > J if all x \in I, y \in J have x > y
   */
  class IntervalGeometricPartialOrder[A: Order] extends PartialOrder[Interval[A]] {
    override def eqv(x: Interval[A], y: Interval[A]): Boolean = x == y

    def partialCompare(i: Interval[A], j: Interval[A]): Double = {
      import Double.NaN
      if (eqv(i, j)) return 0.0
      if (i.isEmpty || j.isEmpty)
        return NaN

      // test if i < j
      (i.upperBound, j.lowerBound) match {
        case (Open(x), Open(y)) if x <= y    => return -1.0
        case (Open(x), Closed(y)) if x <= y  => return -1.0
        case (Closed(x), Open(y)) if x <= y  => return -1.0
        case (Closed(x), Closed(y)) if x < y => return -1.0
        case _                               =>
      }
      // test if i > j
      (i.lowerBound, j.upperBound) match {
        case (Open(x), Open(y)) if x >= y    => return 1.0
        case (Open(x), Closed(y)) if x >= y  => return 1.0
        case (Closed(x), Open(y)) if x >= y  => return 1.0
        case (Closed(x), Closed(y)) if x > y => return 1.0
        case _                               =>
      }
      return NaN
    }
  }

  implicit def intervalGeometricPartialOrder[A: Order]: PartialOrder[Interval[A]] = new IntervalGeometricPartialOrder[A]
}
