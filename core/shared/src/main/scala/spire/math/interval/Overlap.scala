package spire.math.interval

import spire.algebra.{Eq, Order}
import spire.math.Interval

/**
  * An ADT that represents overlapping result for any two intervals.
  * All subclasses and method names are "left-biased":
  * for example, [[StrictlyLess]] result means, that [[lhs]] is strictly less than [[rhs]].
  */
sealed abstract class Overlap[A: Order] extends Serializable {
  def lhs: Interval[A]
  def rhs: Interval[A]
  def isStrictlyLess: Boolean
  def isLessAndOverlaps: Boolean
  def isSubset: Boolean
  def isEqual: Boolean
}

/**
  * Intervals are nonEmpty and don't intersect
  * [[lhs.upperBound]] is strictly less than [[rhs.lowerBound]].
  */
case class StrictlyLess[A: Order] private[spire](lhs: Interval[A], rhs: Interval[A]) extends Overlap[A] {
  def isSubset: Boolean = false
  def isEqual: Boolean = false
  def isStrictlyLess: Boolean = true
  def isLessAndOverlaps: Boolean = false

  /**
    * An interval that joins [[lhs]] and [[rhs]] in a continuous interval without intersecting any of them.
    * For example for (-5, 1] and (4, 6), a join is (1,4]
    */
  def join: Interval[A] = (~lhs).last.intersect((~rhs).head)
}

/**
  * Intermediate abstract class for non-empty overlaps
  */
sealed abstract class Intersects[A: Order] private[spire]() extends Overlap[A] {
  def isStrictlyLess: Boolean = false
}

/**
  * Non empty intervals, for which holds:
  * [[rhs]] ∋ [[lhs.upperBound]] && [[rhs]] ∌ [[lhs.lowerBound]]
  * For example: (-2, 10] and [5, 13)
  */
case class LessAndOverlaps[A: Order] private[spire](lhs: Interval[A], rhs: Interval[A]) extends Intersects[A] {
  def isLessAndOverlaps: Boolean = true
  def isSubset: Boolean = false
  def isEqual: Boolean = false
  def intersection: Interval[A] = lhs.intersect(rhs)
}

/**
  * [[lhs]] is a subset of [[rhs]].
  * Empty interval is always a subset of any other, so all overlaps on empty intervals go here,
  * except `(Ø).overlap(Ø)`, that results in equality.
  *
  * For example [1,4) and [1, 5]
  */
case class Subset[A: Order] private[spire](lhs: Interval[A], rhs: Interval[A]) extends Intersects[A] {
  def isLessAndOverlaps: Boolean = false
  def isGreater: Boolean = false
  def isSuperset: Boolean = false
  def isSubset: Boolean = true
  def isEqual: Boolean = false
  def difference: List[Interval[A]] = rhs -- lhs
}

/**
  * Intervals are equal
  */
case class Equals[A: Order] private[spire](lhs: Interval[A], rhs: Interval[A]) extends Intersects[A] {
  def isLessAndOverlaps: Boolean = false
  def isSubset: Boolean = true
  def isEqual: Boolean = true
}

object Overlap {
  implicit def eqInstance[A: Eq]: Eq[Overlap[A]] = new Eq[Overlap[A]] {
    val eq = Eq[Interval[A]]
    def eqv(x: Overlap[A], y: Overlap[A]): Boolean = (x, y) match {
      case (Equals(x1, y1), Equals(x2, y2)) => eq.eqv(x1, x2) && eq.eqv(y1, y2)
      case (StrictlyLess(x1, y1), StrictlyLess(x2, y2)) => eq.eqv(x1, x2) && eq.eqv(y1, y2)
      case (LessAndOverlaps(x1, y1), LessAndOverlaps(x2, y2)) => eq.eqv(x1, x2) && eq.eqv(y1, y2)
      case (Subset(x1, y1), Subset(x2, y2)) => eq.eqv(x1, x2) && eq.eqv(y1, y2)
      case _ => false
    }
  }
}
