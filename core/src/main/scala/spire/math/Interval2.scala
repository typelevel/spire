package spire.math

import spire.algebra._
import spire.implicits._

/**
 * Interval2 represents a set of values, usually numbers.
 * 
 * Interval2s have upper and lower bounds. Each bound can be one of
 * three kinds:
 * 
 *   * Closed: The boundary value is included in the interval.
 *   * Open: The boundary value is excluded from the interval.
 *   * Unbound: There is no boundary value.
 *
 * When the underlying type of the interval supports it, intervals may
 * be used in arithmetic. There are several possible interpretations
 * of interval arithmetic: the interval can represent uncertainty
 * about a single value (for instance, a quantity +/- tolerance in
 * engineering) or it can represent all values in the interval
 * simultaneously. In this implementation we have chosen to use the
 * probabillistic interpretation.
 *
 * One common pitfall with interval arithmetic is that many familiar
 * algebraic relations do not hold. For instance, given two intervals
 * a and b:
 * 
 *   a == b does not imply a * a == a * b
 *
 * Consider a = b = [-1, 1]. Since any number times itself is
 * non-negative, a * a = [0, 1]. However, a * b = [-1, 1], since we
 * may actually have a=1 and b=-1.
 *
 * These situations will result in loss of precision (in the form of
 * wider intervals). The result is not wrong per se, but less
 * acccurate than it could be.
 */

sealed trait Interval2[A] { lhs =>

  implicit def order: Order[A]

  @inline private[this] final def isClosed(flags: Int): Boolean = flags == 0
  @inline private[this] final def isClosedBelow(flags: Int): Boolean = (flags & 1) == 0
  @inline private[this] final def isClosedAbove(flags: Int): Boolean = (flags & 2) == 0

  @inline private[this] final def isOpen(flags: Int): Boolean = flags == 3
  @inline private[this] final def isOpenBelow(flags: Int): Boolean = (flags & 1) == 1
  @inline private[this] final def isOpenAbove(flags: Int): Boolean = (flags & 2) == 1

  @inline private[this] final def lowerFlag(flags: Int): Int = flags & 1
  @inline private[this] final def upperFlag(flags: Int): Int = flags & 2

  def isEmpty: Boolean = this match {
    case Ranged(lower, upper, flags) => isOpen(flags) && lower === upper
    case _ => false
  }

  def contains(t: A): Boolean =
    isAtOrBelow(t) && isAtOrAbove(t)

  def crosses(t: A) =
    isBelow(t) && isAbove(t)

  def isAbove(t: A): Boolean = this match {
    case Below(upper, flags) => upper > t
    case Ranged(lower, upper, flags) => upper > t
    case _ => true
  }

  def isBelow(t: A): Boolean = this match {
    case Above(lower, flags) => lower < t
    case Ranged(lower, upper, flags) => lower < t
    case _ => true
  }

  def isAtOrAbove(t: A) = this match {
    case Below(upper, flags) =>
      upper > t || isClosedAbove(flags) && upper === t
    case Ranged(lower, upper, flags) =>
      upper > t || isClosedAbove(flags) && upper === t
    case _ =>
      true
  }


  def isAtOrBelow(t: A) = this match {
    case Above(lower, flags) =>
      lower < t || isClosedBelow(flags) && lower === t
    case Ranged(lower, upper, flags) =>
      lower < t || isClosedBelow(flags) && lower === t
    case _ =>
      true
  }


  def isAt(t: A) = this match {
    case Ranged(lower, upper, flags) =>
      isClosed(flags) && lower === t && t === upper
    case _ =>
      false
  }

  private[this] final def maxLower(lower1: A, lower2: A, flags1: Int, flags2: Int): (A, Int) =
    (lower1 compare lower2) match {
      case -1 => (lower2, flags2)
      case 0 => (lower1, flags1 | flags2)
      case 1 => (lower1, flags1)
    }

  private[this] final def minUpper(upper1: A, upper2: A, flags1: Int, flags2: Int): (A, Int) =
    (upper1 compare upper2) match {
      case -1 => (upper1, flags1)
      case 0 => (upper1, flags1 | flags2)
      case 1 => (upper2, flags2)
    }

  def mask(rhs: Interval2[A]): Interval2[A] = lhs match {
    case All() => rhs
    case Below(upper1, flags1) => rhs match {
      case All() =>
        lhs
      case Below(upper2, flags2) =>
        val (u, uf) = minUpper(upper1, upper2, flags1, flags2)
        Below(u, uf)
      case Above(lower2, flags2) =>
        Interval2(lower2, upper1, flags1 | flags2)
      case Ranged(lower2, upper2, flags2) =>
        val (u, uf) = minUpper(upper1, upper2, flags1, flags2)
        Interval2(lower2, u, lowerFlag(flags2) | uf)
    }
    case Above(lower1, flags1) => rhs match {
      case All() =>
        lhs
      case Above(lower2, flags2) =>
        val (l, lf) = maxLower(lower1, lower2, flags1, flags2)
        Above(l, lf)
      case Below(upper2, flags2) =>
        Interval2(lower1, upper2, flags1 | flags2)
      case Ranged(lower2, upper2, flags2) =>
        val (l, lf) = maxLower(lower1, lower2, flags1, flags2)
        Interval2(l, upper2, lf | upperFlag(flags2))
    }
    case Ranged(lower1, upper1, flags1) =>
      rhs match {
        case All() =>
          lhs
        case Above(lower2, flags2) =>
          val (l, lf) = maxLower(lower1, lower2, flags1, flags2)
          Interval2(l, upper1, lf | upperFlag(flags1))
        case Below(upper2, flags2) =>
          val (u, uf) = minUpper(upper1, upper2, flags1, flags2)
          Interval2(lower1, u, lowerFlag(flags1) | uf)
        case Ranged(lower2, upper2, flags2) =>
          val (l, lf) = maxLower(lower1, lower2, flags1, flags2)
          val (u, uf) = minUpper(upper1, upper2, flags1, flags2)
          Interval2(l, u, lf | uf)
      }
  }

  def split(t: A): (Interval2[A], Interval2[A]) =
    (this mask Interval2.below(t), this mask Interval2.above(t))

  def splitAtZero(implicit ev: Ring[A]): (Interval2[A], Interval2[A]) =
    split(ev.zero)

  def mapAroundZero[B](f: Interval2[A] => B)(implicit ev: Ring[A]): (B, B) =
    splitAtZero match {
      case (a, b) => (f(a), f(b))
    }

  override def toString(): String = this match {
    case All() =>
      "(-∞, ∞)"
    case Above(lower, flags) =>
      if (isClosedBelow(flags)) s"[$lower, ∞)" else s"($lower, ∞)"
    case Below(upper, flags) =>
      if (isClosedAbove(flags)) s"(-∞, $upper]" else s"(-∞, $upper)"
    case Ranged(lower, upper, flags) =>
      val s1 = if (isClosedBelow(flags)) s"[$lower" else s"($lower"
      val s2 = if (isClosedAbove(flags)) s"$upper]" else s"$upper)"
      s"$s1, $s2"
  }
}

case class All[A] private[spire] (implicit ev: Order[A]) extends Interval2[A] {
  val order = ev
}

case class Above[A] private[spire] (lower: A, flags: Int)(implicit ev: Order[A]) extends Interval2[A] {
  val order = ev
}

case class Below[A] private[spire] (upper: A, flags: Int)(implicit ev: Order[A]) extends Interval2[A] {
  val order = ev
}

case class Ranged[A] private[spire] (lower: A, upper: A, flags: Int)(implicit ev: Order[A]) extends Interval2[A] {
  val order = ev
}

object Interval2 {
  def empty[A: Order]: Interval2[A] =
    Ranged(null.asInstanceOf[A], null.asInstanceOf[A], 3) //fixme

  def apply[A: Order](lower: A, upper: A, flags: Int): Interval2[A] =
    if (lower <= upper) Ranged(lower, upper, flags) else Interval2.empty[A]

  def above[A: Order](a: A): Interval2[A] = Above(a, 1)
  def below[A: Order](a: A): Interval2[A] = Below(a, 2)
  def atOrAbove[A: Order](a: A): Interval2[A] = Above(a, 0)
  def atOrBelow[A: Order](a: A): Interval2[A] = Below(a, 0)
}
