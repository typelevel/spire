package spire
package optional

import spire.algebra.{Order, PartialOrder}
import spire.math.{Interval, Point}
import spire.math.interval._
import spire.syntax.order._

object intervalValuePartialOrder {

  /** Interval partial order defined as follows:
   *
   * I <= J if I is a subset of J.
   */
  class IntervalValuePartialOrder[A: Order] extends PartialOrder[Interval[A]] {
    override def eqv(x: Interval[A], y: Interval[A]): Boolean =
      (x, y) match {
        case (Point(p1), Point(p2)) => p1 === p2
        case _ => false
      }

    override def lteqv(x: Interval[A], y: Interval[A]): Boolean =
      x.upperBound match {
        case v1: ValueBound[A] =>
          y.lowerBound match {
            case v2: ValueBound[A] => v1.a <= v2.a
            case _ => false
          }
        case _ =>
          false
      }

    override def lt(x: Interval[A], y: Interval[A]): Boolean =
      x.upperBound match {
        case Open(a1) =>
          y.lowerBound match {
            case v2: ValueBound[A] => a1 <= v2.a
            case _ => false
          }
        case Closed(a1) =>
          y.lowerBound match {
            case Closed(a2) => a1 < a2
            case Open(a2) => a1 <= a2
            case _ => false
          }
        case _ => false
      }

    override def gteqv(x: Interval[A], y: Interval[A]): Boolean = lteqv(y, x)
    override def gt(x: Interval[A], y: Interval[A]): Boolean = lt(y, x)

    def partialCompare(x: Interval[A], y: Interval[A]): Double =
      if (eqv(x, y)) 0.0
      else if (lt(x, y)) -1.0
      else if (gt(x, y)) 1.0
      else Double.NaN
  }

  implicit def intervalValuePartialOrder[A: Order]: PartialOrder[Interval[A]] =
    new IntervalValuePartialOrder[A]
}
