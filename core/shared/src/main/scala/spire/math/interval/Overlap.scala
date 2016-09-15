package spire.math.interval

import spire.algebra.Eq
import spire.math.Interval
import spire.math.interval.Overlap.{Disjoint, Equal, Subset}

/**
  * An ADT that represents overlapping result for any two intervals.
  */
sealed abstract class Overlap[A] extends Product with Serializable {
  def isDisjoint: Boolean = this.isInstanceOf[Disjoint[_]]
  def isSubset: Boolean = this match {
    case Subset(_, _) | Equal() => true
    case _ => false
  }
  def isEqual: Boolean = this.isInstanceOf[Equal[_]]
}

object Overlap {
  /**
    * Intervals are nonEmpty and don't intersect
    * [[lower.upperBound]] is strictly less than [[upper.lowerBound]].
    */
  case class Disjoint[A] private[spire](lower: Interval[A], upper: Interval[A]) extends Overlap[A] {

    /**
      * An interval that joins [[lower]] and [[upper]] in a continuous interval without intersecting any of them.
      * For example for (-5, 1] and (4, 6), a join is (1,4]
      */
    def join: Interval[A] = (~lower).last.intersect((~upper).head)
  }

  /**
    * Non empty intervals, for which holds:
    * [[upper]] ∋ [[lower.upperBound]] && [[upper]] ∌ [[lower.lowerBound]]
    * For example: (-2, 10] and [5, 13)
    */
  case class PartialOverlap[A] private[spire](lower: Interval[A], upper: Interval[A]) extends Overlap[A]

  /**
    * [[inner]] is a subset of [[outer]].
    * Empty interval is always a subset of any other, so all overlaps on empty intervals go here,
    * except `(Ø).overlap(Ø)`, that results in equality.
    *
    * For example [1,4) and [1, 5]
    */
  case class Subset[A] private[spire](inner: Interval[A], outer: Interval[A]) extends Overlap[A]

  /**
    * Intervals are equal
    */
  case class Equal[A] private[spire]() extends Overlap[A]


  implicit def eqInstance[A: Eq]: Eq[Overlap[A]] = new Eq[Overlap[A]] {
    val eq = Eq[Interval[A]]
    def eqv(x: Overlap[A], y: Overlap[A]): Boolean = (x, y) match {
      case (Equal(), Equal()) => true
      case (Disjoint(x1, y1), Disjoint(x2, y2)) => eq.eqv(x1, x2) && eq.eqv(y1, y2)
      case (PartialOverlap(x1, y1), PartialOverlap(x2, y2)) => eq.eqv(x1, x2) && eq.eqv(y1, y2)
      case (Subset(x1, y1), Subset(x2, y2)) => eq.eqv(x1, x2) && eq.eqv(y1, y2)
      case _ => false
    }
  }
}
