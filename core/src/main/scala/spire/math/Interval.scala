package spire
package math

import Predef.{any2stringadd => _, _}

import spire.algebra._
import spire.math.poly.Term
import spire.math.interval._
import spire.math.interval.Bound._
import spire.syntax.field._
import spire.syntax.nroot._
import spire.syntax.order._

import java.lang.Double.isNaN

/**
 * Interval represents a set of values, usually numbers.
 *
 * Intervals have upper and lower bounds. Each bound can be one of
 * four kinds:
 *
 *  * Closed: The boundary value is included in the interval.
 *  * Open: The boundary value is excluded from the interval.
 *  * Unbound: There is no boundary value.
 *  * Empty: The interval itself is empty.
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
 * accurate than it could be.
 *
 * These intervals should not be used with floating point bounds,
 * as proper rounding is not implemented. Generally, the JVM is
 * not an easy platform to perform robust arithmetic, as the
 * IEEE 754 rounding modes cannot be set.
 */
sealed abstract class Interval[A] extends Serializable { lhs =>

  @inline protected[this] final def isClosed(flags: Int): Boolean = flags == 0
  @inline protected[this] final def isClosedLower(flags: Int): Boolean = (flags & 1) == 0
  @inline protected[this] final def isClosedUpper(flags: Int): Boolean = (flags & 2) == 0

  @inline protected[this] final def isOpen(flags: Int): Boolean = flags == 3
  @inline protected[this] final def isOpenLower(flags: Int): Boolean = (flags & 1) == 1
  @inline protected[this] final def isOpenUpper(flags: Int): Boolean = (flags & 2) == 2

  @inline protected[this] final def lowerFlag(flags: Int): Int = flags & 1
  @inline protected[this] final def upperFlag(flags: Int): Int = flags & 2

  @inline protected[this] final def reverseLowerFlag(flags: Int): Int = flags ^ 1
  @inline protected[this] final def reverseUpperFlag(flags: Int): Int = flags ^ 2
  @inline protected[this] final def reverseFlags(flags: Int): Int = flags ^ 3

  protected[this] final def lowerFlagToUpper(flags: Int): Int = (flags & 1) << 1
  protected[this] final def upperFlagToLower(flags: Int): Int = (flags & 2) >>> 1

  @inline protected[this] final def swapFlags(flags: Int): Int =
    ((flags & 1) << 1) | ((flags & 2) >>> 1)

  protected[this] def lowerPairBelow(lower1: A, flags1: Int, lower2: A, flags2: Int)(implicit o: Order[A]): Boolean =
    lower1 < lower2 || lower1 === lower2 && (isClosedLower(flags1) || isOpenLower(flags2))

  protected[this] def upperPairAbove(upper1: A, flags1: Int, upper2: A, flags2: Int)(implicit o: Order[A]): Boolean =
    upper1 > upper2 || upper1 === upper2 && (isClosedUpper(flags1) || isOpenUpper(flags2))

  def isEmpty: Boolean =
    this.isInstanceOf[Empty[_]]

  def nonEmpty: Boolean =
    !isEmpty

  def isPoint: Boolean =
    this.isInstanceOf[Point[_]]

  def contains(t: A)(implicit o: Order[A]): Boolean =
    hasAtOrBelow(t) && hasAtOrAbove(t)

  def doesNotContain(t: A)(implicit o: Order[A]): Boolean =
    !hasAtOrBelow(t) || !hasAtOrAbove(t)

  def crosses(t: A)(implicit o: Order[A]): Boolean =
    hasBelow(t) && hasAbove(t)

  def crossesZero(implicit o: Order[A], ev: AdditiveMonoid[A]): Boolean =
    hasBelow(ev.zero) && hasAbove(ev.zero)

  def isBounded: Boolean =
    this match {
      case Below(_, _) | Above(_, _) | All() => false
      case _ => true
    }

  def lowerBound: Bound[A]

  def upperBound: Bound[A]

  def mapBounds[B: Order](f: A => B): Interval[B] =
    Interval.fromBounds(lowerBound.map(f), upperBound.map(f))

  def fold[B](f: (Bound[A], Bound[A]) => B): B =
    f(lowerBound, upperBound)

  def isSupersetOf(rhs: Interval[A])(implicit o: Order[A]): Boolean = (lhs, rhs) match {
    // deal with All, Empty and Point on either left or right side

    case (All(), _) => true
    case (_, All()) => false

    case (_, Empty()) => true
    case (Empty(), _) => false

    case (Point(lhsval), Point(rhsval)) => lhsval === rhsval
    case (Point(_), _) => false // rhs cannot be Empty or Point
    case (_, Point(rhsval)) => lhs.contains(rhsval)

    // remaining cases are Above, Below and Bounded, we deal first with the obvious false

    case (Above(_, _), Below(_, _)) => false
    case (Below(_, _), Above(_, _)) => false
    case (Bounded(_, _, _), Below(_, _)) => false
    case (Bounded(_, _, _), Above(_, _)) => false

    case (Above(lower1, flags1), Bounded(lower2, _, flags2)) =>
      lowerPairBelow(lower1, flags1, lower2, flags2)
    case (Above(lower1, flags1), Above(lower2, flags2)) =>
      lowerPairBelow(lower1, flags1, lower2, flags2)

    case (Below(upper1, flags1), Below(upper2, flags2)) =>
      upperPairAbove(upper1, flags1, upper2, flags2)
    case (Below(upper1, flags1), Bounded(_, upper2, flags2)) =>
      upperPairAbove(upper1, flags1, upper2, flags2)

    case (Bounded(lower1, upper1, flags1), Bounded(lower2, upper2, flags2)) =>
      lowerPairBelow(lower1, flags1, lower2, flags2) &&
        upperPairAbove(upper1, flags1, upper2, flags2)
  }

  def isProperSupersetOf(rhs: Interval[A])(implicit o: Order[A]): Boolean =
    lhs != rhs && (lhs isSupersetOf rhs)

  def isSubsetOf(rhs: Interval[A])(implicit o: Order[A]): Boolean =
    rhs isSupersetOf lhs

  def isProperSubsetOf(rhs: Interval[A])(implicit o: Order[A]): Boolean =
    rhs isProperSupersetOf lhs

  // Does this interval contain any points above t ?
  def hasAbove(t: A)(implicit o: Order[A]): Boolean = this match {
    case Empty() => false
    case Point(p) => p > t
    case Below(upper, _) => upper > t
    case Bounded(_, upper, _) => upper > t
    case All() => true
    case Above(_, _) => true
  }

  // Does this interval contain any points below t ?
  def hasBelow(t: A)(implicit o: Order[A]): Boolean = this match {
    case Empty() => false
    case Point(p) => p < t
    case Above(lower, _) => lower < t
    case Bounded(lower, _, _) => lower < t
    case Below(_, _) => true
    case All() => true
  }

  // Does this interval contains any points at or above t ?
  def hasAtOrAbove(t: A)(implicit o: Order[A]): Boolean = this match {
    case _: Empty[_] => false
    case Point(p) => p >= t
    case Below(upper, flags) =>
      upper > t || isClosedUpper(flags) && upper === t
    case Bounded(lower, upper, flags) =>
      upper > t || isClosedUpper(flags) && upper === t
    case _: Above[_] => true
    case _: All[_] => true
  }

  // Does this interval contains any points at or below t ?
  def hasAtOrBelow(t: A)(implicit o: Order[A]): Boolean = this match {
    case _: Empty[_] => false
    case Point(p) => p <= t
    case Above(lower, flags) =>
      lower < t || isClosedLower(flags) && lower === t
    case Bounded(lower, upper, flags) =>
      lower < t || isClosedLower(flags) && lower === t
    case _: Below[_] => true
    case _: All[_] => true
  }

  def isAt(t: A)(implicit o: Eq[A]): Boolean = this match {
    case Point(p) => t === p
    case _ => false
  }

  def intersects(rhs: Interval[A])(implicit o: Order[A]): Boolean =
    !(lhs intersect rhs).isEmpty

  def &(rhs: Interval[A])(implicit o: Order[A]): Interval[A] =
    lhs intersect rhs

  def intersect(rhs: Interval[A])(implicit o: Order[A]): Interval[A] =
    Interval.fromBounds(maxLower(lhs.lowerBound, rhs.lowerBound, true),
      minUpper(lhs.upperBound, rhs.upperBound, true))

  /* Compute the set complementary to this interval. That set is return as a possibly empty list of non-empty intervals */
  def unary_~(implicit o: Order[A]): List[Interval[A]] =
    this match {
      case All() =>
        Nil
      case Empty() =>
        List(All())
      case Above(lower, lf) =>
        List(Below(lower, lowerFlagToUpper(reverseLowerFlag(lf))))
      case Below(upper, uf) =>
        List(Above(upper, upperFlagToLower(reverseUpperFlag(uf))))
      case Point(p) =>
        List(Interval.below(p), Interval.above(p))
      case Bounded(lower, upper, flags) =>
        val lx = lowerFlagToUpper(reverseLowerFlag(lowerFlag(flags)))
        val ux = upperFlagToLower(reverseUpperFlag(upperFlag(flags)))
        List(Below(lower, lx), Above(upper, ux))
    }

  /* Returns the list of disjoint non-empty intervals resulting from the exclusion of the interval with rhs */
  def --(rhs: Interval[A])(implicit o: Order[A]): List[Interval[A]] =
    if (lhs intersects rhs) {
      (~rhs).map(lhs & _).filter(_.nonEmpty)
    } else {
      if (lhs.isEmpty) Nil else List(lhs)
    }

  def split(t: A)(implicit o: Order[A]): (Interval[A], Interval[A]) =
    (this intersect Interval.below(t), this intersect Interval.above(t))

  def splitAtZero(implicit o: Order[A], ev: AdditiveMonoid[A]): (Interval[A], Interval[A]) =
    split(ev.zero)

  def mapAroundZero[B](f: Interval[A] => B)(implicit o: Order[A], ev: AdditiveMonoid[A]): (B, B) =
    splitAtZero match {
      case (a, b) => (f(a), f(b))
    }

  def |(rhs: Interval[A])(implicit o: Order[A]): Interval[A] =
    lhs union rhs

  def union(rhs: Interval[A])(implicit o: Order[A]): Interval[A] =
    Interval.fromBounds(minLower(lhs.lowerBound, rhs.lowerBound, false),
      maxUpper(lhs.upperBound, rhs.upperBound, false))

  override def toString(): String = this match {
    case All() =>
      "(-∞, ∞)"
    case Empty() =>
      "(Ø)"
    case Above(lower, flags) =>
      if (isClosedLower(flags)) s"[$lower, ∞)" else s"($lower, ∞)"
    case Below(upper, flags) =>
      if (isClosedUpper(flags)) s"(-∞, $upper]" else s"(-∞, $upper)"
    case Point(p) =>
      s"[$p]"
    case Bounded(lower, upper, flags) =>
      val s1 = if (isClosedLower(flags)) s"[$lower" else s"($lower"
      val s2 = if (isClosedUpper(flags)) s"$upper]" else s"$upper)"
      s"$s1, $s2"
  }

  def abs(implicit o: Order[A], m: AdditiveGroup[A]): Interval[A] =
    if (crossesZero) { // only Bounded, Above or Below can cross zero
      this match {
        case Bounded(lower, upper, fs) =>
          val x = -lower
          if (x > upper) Bounded(m.zero, x, lowerFlagToUpper(fs))
          else if (upper > x) Bounded(m.zero, upper, upperFlag(fs))
          else Bounded(m.zero, x, lowerFlagToUpper(fs) & upperFlag(fs))
        case _ => // Above or Below
          Interval.atOrAbove(m.zero)
      }
    } else if (hasBelow(m.zero)) {
      -this
    } else {
      this
    }

  // for all a in A, and all b in B, (A vmin B) is the interval that contains all (a min b)
  def vmin(rhs: Interval[A])(implicit o: Order[A], m: AdditiveMonoid[A]): Interval[A] =
    Interval.fromBounds(minLower(lhs.lowerBound, rhs.lowerBound, true),
      minUpper(lhs.upperBound, rhs.upperBound, true))

  // for all a in A, and all b in B, (A vmax B) is the interval that contains all (a max b)
  def vmax(rhs: Interval[A])(implicit o: Order[A], m: AdditiveMonoid[A]): Interval[A] =
    Interval.fromBounds(maxLower(lhs.lowerBound, rhs.lowerBound, true),
      maxUpper(lhs.upperBound, rhs.upperBound, true))

  def combine(rhs: Interval[A])(f: (A, A) => A)(implicit o: Order[A]): Interval[A] = {
    val lb = lhs.lowerBound.combine(rhs.lowerBound)(f)
    val ub = lhs.upperBound.combine(rhs.upperBound)(f)
    Interval.fromBounds(lb, ub)
  }

  def +(rhs: Interval[A])(implicit o: Order[A], ev: AdditiveSemigroup[A]): Interval[A] =
    combine(rhs)(_ + _)

  def -(rhs: Interval[A])(implicit o: Order[A], ev: AdditiveGroup[A]): Interval[A] =
    lhs + (-rhs)

  // scalastyle:off method.length
  def *(rhs: Interval[A])(implicit o: Order[A], ev: Semiring[A]): Interval[A] = {
    val z = ev.zero

    def aboveAbove(lower1: A, lf1: Int, lower2: A, lf2: Int): Interval[A] = {
      val lower1s = lower1.compare(z)
      val lower2s = lower2.compare(z)

      if (lower1s < 0 || lower2s < 0) All() else {
        val strongZero = (lower1s == 0 && isClosedLower(lf1)) || (lower2s == 0 && isClosedLower(lf2))
        val flags = if (strongZero) 0 else lf1 | lf2
        Above(lower1 * lower2, flags)
      }
    }

    def belowBelow(upper1: A, uf1: Int, upper2: A, uf2: Int): Interval[A] = {
      val upper1s = upper1.compare(z)
      val upper2s = upper2.compare(z)
      if (upper1s > 0 || upper2s > 0) All() else {
        val strongZero = (upper1s == 0 && isClosedUpper(uf1)) || (upper2s == 0 && isClosedUpper(uf2))
        val flags = if (strongZero) 0 else upperFlagToLower(uf1) | upperFlagToLower(uf2)
        Above(upper1 * upper2, flags)
      }
    }

    def aboveBelow(lower1: A, lf1: Int, upper2: A, uf2: Int): Interval[A] = {
      val lower1s = lower1.compare(z)
      val upper2s = upper2.compare(z)
      if (lower1s < 0 || upper2s > 0) All() else {
        val strongZero = (lower1s == 0 && isClosedLower(lf1)) || (upper2s == 0 && isClosedUpper(uf2))
        val flags = if (strongZero) 0 else lowerFlagToUpper(lf1) | uf2
        Below(lower1 * upper2, flags)
      }
    }

    def aboveBounded(lower1: A, lf1: Int, lower2: A, upper2: A, flags2: Int): Interval[A] = {
      val lower1s = lower1.compare(z)
      val lower2s = lower2.compare(z)
      val upper2s = upper2.compare(z)
      val hasBelowZero1 = lower1s < 0
      val hasBelowZero2 = lower2s < 0
      val hasAboveZero2 = upper2s > 0
      if (hasBelowZero2 && hasAboveZero2) All() // bounded interval crosses zero

      else if (hasAboveZero2) { // bounded interval is fully above zero
        if (hasBelowZero1) // the minimal point is lower1(-) * upper2(+)
          Above(lower1 * upper2, lf1 | upperFlagToLower(flags2))
        else { // the minimal point is lower1(+) * lower2(+)
          val strongZero = (lower1s == 0 && isClosedLower(lf1)) || (lower2s == 0 && isClosedLower(flags2))
          val flags = if (strongZero) 0 else lf1 | lowerFlag(flags2)
          Above(lower1 * lower2, flags)
        }
      } else { // bounded interval is fully below zero
        assert(hasBelowZero2)
        if (hasBelowZero1) { // the maximal point is lower1(-) * lower2(-)
          val strongZero = (lower1s == 0 && isClosedLower(lf1)) || (lower2s == 0 && isClosedLower(flags2))
          val flags = if (strongZero) 0 else lowerFlagToUpper(lf1) | lowerFlagToUpper(flags2)
          Below(lower1 * lower2, flags)
        }
        else { // the maximal point is lower1(+) * upper2(-)
          val strongZero = (lower1s == 0 && isClosedLower(lf1)) || (upper2s == 0 && isClosedUpper(flags2))
          val flags = if (strongZero) 0 else lowerFlagToUpper(lf1) | upperFlag(flags2)
          Below(lower1 * upper2, flags)
        }
      }
    }

    def belowBounded(upper1: A, uf1: Int, lower2: A, upper2: A, flags2: Int): Interval[A] = {
      val upper1s = upper1.compare(z)
      val lower2s = lower2.compare(z)
      val upper2s = upper2.compare(z)
      val hasAboveZero1 = upper1s > 0
      val hasBelowZero2 = lower2s < 0
      val hasAboveZero2 = upper2s > 0
      if (hasBelowZero2 && hasAboveZero2) All() // bounded interval crosses zero

      else if (hasAboveZero2) { // bounded interval is fully above zero
        if (hasAboveZero1) // the maximal point is upper1(+) * upper2(+)
          Below(upper1 * upper2, uf1 | upperFlag(flags2))
        else { // the maximal point is upper1(+) * lower2(-)
          val strongZero = (upper1s == 0 && isClosedUpper(uf1)) || (lower2s == 0 && isClosedLower(flags2))
          val flags = if (strongZero) 0 else uf1 | lowerFlagToUpper(flags2)
          Below(upper1 * lower2, flags)
        }
      } else { // bounded interval is fully below zero
        if (hasAboveZero1) { // the minimal point is upper1(+) * lower2(-)
          val strongZero = (lower2s == 0 && isClosedLower(flags2))
          val flags = if (strongZero) 0 else upperFlagToLower(uf1) | lowerFlag(flags2)
          Above(upper1 * lower2, flags)
        }
        else { // the minimal point is upper1(-) * upper2(-)
          val strongZero = (upper1s == 0 && isClosedUpper(uf1)) || (upper2s == 0 && isClosedUpper(flags2))
          val flags = if (strongZero) 0 else upperFlagToLower(uf1) | upperFlagToLower(flags2)
          Above(upper1 * upper2, flags)
        }
      }
    }

    def boundedBounded(bd1: Bounded[A], bd2: Bounded[A]): Interval[A] = {
      val lb1 = bd1.lowerBound
      val ub1 = bd1.upperBound
      val lb2 = bd2.lowerBound
      val ub2 = bd2.upperBound
      val lb1sz = lb1.a === z && lb1.isClosed
      val lb2sz = lb2.a === z && lb2.isClosed
      val ub1sz = ub1.a === z && ub1.isClosed
      val ub2sz = ub2.a === z && ub2.isClosed

      val ll = if (lb1sz || lb2sz) interval.Closed(z) else (lb1 *~ lb2)
      val lu = if (lb1sz || ub2sz) interval.Closed(z) else (lb1 *~ ub2)
      val ul = if (ub1sz || lb2sz) interval.Closed(z) else (ub1 *~ lb2)
      val uu = if (ub1sz || ub2sz) interval.Closed(z) else (ub1 *~ ub2)
      ValueBound.union4(ll, lu, ul, uu)
    }


    (lhs, rhs) match {
      case (Empty(), _) => lhs
      case (_, Empty()) => rhs
      case (Point(lv), _) => rhs * lv // use multiplication by scalar
      case (_, Point(rv)) => lhs * rv
      // now lhs and rhs are both intervals with more that one point
      case (All(), _) => lhs
      case (_, All()) => rhs

      case (Above(lower1, lf1), Above(lower2, lf2)) => aboveAbove(lower1, lf1, lower2, lf2)
      case (Above(lower1, lf1), Below(upper2, uf2)) => aboveBelow(lower1, lf1, upper2, uf2)
      case (Below(upper1, uf1), Above(lower2, lf2)) => aboveBelow(lower2, lf2, upper1, uf1)
      case (Below(upper1, uf1), Below(upper2, uf2)) => belowBelow(upper1, uf1, upper2, uf2)

      case (Above(lower1, lf1), Bounded(lower2, upper2, flags2)) =>
        aboveBounded(lower1, lf1, lower2, upper2, flags2)
      case (Bounded(lower1, upper1, flags1), Above(lower2, lf2)) =>
        aboveBounded(lower2, lf2, lower1, upper1, flags1)
      case (Below(upper1, uf1), Bounded(lower2, upper2, flags2)) =>
        belowBounded(upper1, uf1, lower2, upper2, flags2)
      case (Bounded(lower1, upper1, flags1), Below(upper2, uf2)) =>
        belowBounded(upper2, uf2, lower1, upper1, flags1)

      case (bd1: Bounded[A], bd2: Bounded[A]) => boundedBounded(bd1, bd2)
    }
  }

  def reciprocal(implicit o: Order[A], ev: Field[A]): Interval[A] = {
    val z = ev.zero
    def error: Nothing = throw new java.lang.ArithmeticException("/ by zero")

    this match {
      case All() => error
      case Empty() => this

      case Above(lower, lf) =>
        (lower.compare(z), isClosedLower(lf)) match {
          case (x, _) if x < 0 => error // crosses zero
          case (0, true) => error // contains zero
          case (0, false) => this
          case _ => Bounded(z, lower.reciprocal, 1 | lowerFlagToUpper(lf))
        }

      case Below(upper, uf) =>
        (upper.compare(z), isClosedUpper(uf)) match {
          case (x, _) if x > 0 => error // crosses zero
          case (0, true) => error // contains zero
          case (0, false) => this
          case _ => Bounded(upper.reciprocal, z, 2 | upperFlagToLower(uf))
        }

      case Point(v) => Point(v.reciprocal)

      case Bounded(lower, upper, flags) =>
        (lower.compare(z), upper.compare(z), isClosedLower(flags), isClosedUpper(flags)) match {
          case (x, y, _, _) if x < 0 && y > 0 => error // crosses zero
          case (0, _, true, _) => error // contains zero
          case (_, 0, _, true) => error // contains zero
          case (0, _, false, _) => Above(upper.reciprocal, upperFlagToLower(flags))
          case (_, 0, _, false) => Below(lower.reciprocal, lowerFlagToUpper(flags))
          case _ => Bounded(upper.reciprocal, lower.reciprocal, swapFlags(flags))
        }
    }
  }
  // scalastyle:on method.length

  def /(rhs: Interval[A])(implicit o: Order[A], ev: Field[A]): Interval[A] =
    (lhs, rhs) match {
      case (Point(lv), _) => rhs.reciprocal * lv
      case (_, Point(rv)) => lhs * rv.reciprocal
      case (_, _) => lhs * rhs.reciprocal
    }

  def /(rhs: A)(implicit o: Order[A], ev: Field[A]): Interval[A] =
    lhs * rhs.reciprocal

  def +(rhs: A)(implicit ev: AdditiveSemigroup[A]): Interval[A] =
    this match {
      case Point(v) => Point(v + rhs)
      case Bounded(l, u, flags) => Bounded(l + rhs, u + rhs, flags)
      case Above(l, lf) => Above(l + rhs, lf)
      case Below(u, uf) => Below(u + rhs, uf)
      case All() | Empty() => this
    }

  def -(rhs: A)(implicit ev: AdditiveGroup[A]): Interval[A] =
    this + (-rhs)

  def unary_-(implicit ev: AdditiveGroup[A]): Interval[A] =
    this match {
      case Point(v) => Point(-v)
      case Bounded(l, u, f) => Bounded(-u, -l, swapFlags(f))
      case Above(l, lf) => Below(-l, lowerFlagToUpper(lf))
      case Below(u, uf) => Above(-u, upperFlagToLower(uf))
      case All() | Empty() => this
    }

  def *(rhs: A)(implicit o: Order[A], ev: Semiring[A]): Interval[A] =
    if (rhs < ev.zero) {
      this match {
        case Point(v) => Point(v * rhs)
        case Bounded(l, u, f) => Bounded(u * rhs, l * rhs, swapFlags(f))
        case Above(l, lf) => Below(l * rhs, lowerFlagToUpper(lf))
        case Below(u, uf) => Above(u * rhs, upperFlagToLower(uf))
        case All() | Empty() => this
      }
    } else if (rhs === ev.zero) {
      Interval.zero
    } else {
      this match {
        case Point(v) => Point(v * rhs)
        case Bounded(l, u, flags) => Bounded(l * rhs, u * rhs, flags)
        case Above(l, lf) => Above(l * rhs, lf)
        case Below(u, uf) => Below(u * rhs, uf)
        case All() | Empty() => this
      }
    }

  def pow(k: Int)(implicit o: Order[A], r: Ring[A]): Interval[A] = {
    def loop(b: Interval[A], k: Int, extra: Interval[A]): Interval[A] =
      if (k == 1)
        b * extra
      else
        loop(b * b, k >>> 1, if ((k & 1) == 1) b * extra else extra)

    if (k < 0) {
      throw new IllegalArgumentException(s"negative exponent: $k")
    } else if (k == 0) {
      Interval.point(r.one)
    } else if (k == 1) {
      this
    } else if ((k & 1) == 0) {
      val t = abs
      loop(t, k - 1, t)
    } else {
      loop(this, k - 1, this)
    }
  }

  def nroot(k: Int)(implicit o: Order[A], r: Ring[A], n: NRoot[A]): Interval[A] = {
    if (k == 1) {
      this
    } else if ((k & 1) == 0 && hasBelow(r.zero)) {
      throw new IllegalArgumentException("can't take even root of negative number")
    } else {
      this match {
        case All() | Empty() => this
        case Point(v) => Point(v.nroot(k))
        case Above(l, lf) => Above(l.nroot(k), lf)
        case Below(u, uf) => Below(u.nroot(k), uf)
        case Bounded(l, u, flags) => Bounded(l.nroot(k), u.nroot(k), flags)
      }
    }
  }

  def sqrt(implicit o: Order[A], r: Ring[A], n: NRoot[A]): Interval[A] = nroot(2)

  def top(epsilon: A)(implicit r: AdditiveGroup[A]): Option[A] =
    this match {
      case Empty() | All() | Above(_, _) =>
        None // TOCHECK: changed semantics, Empty().top == None
      case Below(upper, uf) =>
        Some(if (isOpenUpper(uf)) upper - epsilon else upper)
      case Point(v) =>
        Some(v)
      case Bounded(_, upper, flags) =>
        Some(if (isOpenUpper(flags)) upper - epsilon else upper)
    }

  def bottom(epsilon: A)(implicit r: AdditiveGroup[A]): Option[A] =
    this match {
      case Empty() | All() | Below(_, _) =>
        None // TOCHECK: changed semantics, Empty().bottom == None
      case Above(lower, lf) =>
        Some(if (isOpenLower(lf)) lower + epsilon else lower)
      case Point(v) =>
        Some(v)
      case Bounded(lower, _, flags) =>
        Some(if (isOpenLower(flags)) lower + epsilon else lower)
    }

  import spire.random.{Dist, Uniform}

  def dist(min: A, max: A, epsilon: A)(implicit u: Uniform[A], r: AdditiveGroup[A]): Dist[A] =
    u(bottom(epsilon).getOrElse(min), top(epsilon).getOrElse(max))

  /**
   * Apply the given polynomial to the interval.
   *
   * For every point contained in the interval, this method maps that
   * point through the given polynomial. The resulting interval is the
   * set of all the translated points. I.e.
   *
   *     result = { p(x) | x ∈ interval }
   *
   */
  def translate(p: Polynomial[A])(implicit o: Order[A], ev: Field[A]): Interval[A] = {
    val terms2 = p.terms.map { case Term(c, e) => Term(Interval.point(c), e) }
    val p2 = Polynomial(terms2)
    p2(this)
  }

  // optional unicode operators

  /* Indicates whether the interval contains rhs */
  def ∋(rhs: A)(implicit o: Order[A]): Boolean = lhs contains rhs
  /* Indicates whether the interval doesn't contain rhs */
  def ∌(rhs: A)(implicit o: Order[A]): Boolean = !(lhs contains rhs)

  /* Indicates whether a is contained in the interval */
  def ∈:(a: A)(implicit o: Order[A]): Boolean = lhs contains a
  /* Indicates whether a is not contained in the interval */
  def ∉:(a: A)(implicit o: Order[A]): Boolean = !(lhs contains a)

  /* Returns the intersection of the interval with rhs */
  def ∩(rhs: Interval[A])(implicit o: Order[A]): Interval[A] = lhs intersect rhs
  /* Returns the union of the interval with rhs */
  def ∪(rhs: Interval[A])(implicit o: Order[A]): Interval[A] = lhs union rhs
  /* Returns the list of disjoint non-empty intervals resulting from the exclusion of the interval with rhs */
  def \(rhs: Interval[A])(implicit o: Order[A]): List[Interval[A]] = lhs -- rhs

  /* Indicates whether the interval is a strict subset of rhs */
  def ⊂(rhs: Interval[A])(implicit o: Order[A]): Boolean = lhs isProperSubsetOf rhs
  /* Indicates whether rhs is a strict subset of the interval*/
  def ⊃(rhs: Interval[A])(implicit o: Order[A]): Boolean = lhs isProperSupersetOf rhs

  /* Indicates whether rhs is a subset of the interval */
  def ⊆(rhs: Interval[A])(implicit o: Order[A]): Boolean = lhs isSubsetOf rhs
  /* Indicates whether rhs is a subset of the interval */
  def ⊇(rhs: Interval[A])(implicit o: Order[A]): Boolean = lhs isSupersetOf rhs

  // xyz

  // find the "first" value in our iterator. if step is positive we
  // proceed from the lower bound up, and if negative from the upper
  // bound down. thus, we always use addition when dealing with the
  // step.
  private[this] def getStart(bound: Bound[A], step: A, unboundError: String)(implicit ev: AdditiveMonoid[A]): A =
    bound match {
      case EmptyBound() => ev.zero
      case Open(x) => x + step
      case Closed(x) => x
      case Unbound() => throw new IllegalArgumentException(unboundError)
    }

  /**
   * Build an Iterator[A] from an Interval[A] and a (step: A)
   * parameter.
   *
   * A positive 'step' means we are proceeding from the lower bound
   * up, and a negative 'step' means we are proceeding from the upper
   * bound down. In each case, the interval must be bounded on the
   * side we are starting with (though it may be unbound on the
   * opposite side). A zero 'step' is not allowed.
   *
   * The step is repeatedly added to the starting parameter as long as
   * the sum remains within the interval. This means that arithmetic
   * error can accumulate (e.g. with doubles). However, this method
   * does overflow checking to ensure that Intervals parameterized on
   * integer types will behave correctly.
   *
   * Users who want to avoid using arithmetic error should consider
   * starting with an Interval[Rational], calling iterator with the
   * exact step desired, then mapping to the original type
   * (e.g. Double). For example:
   *
   *     val ns = Interval.closed(Rational(0), Rational(5))
   *     val it = ns.iterator(Rational(1, 7)).map(_.toDouble)
   *
   * This method provides some of the same functionality as Scala's
   * NumericRange class.
   */
  def iterator(step: A)(implicit o: Order[A], ev: AdditiveMonoid[A], nt: NumberTag[A]): Iterator[A] = {

    // build an iterator, using start, step, and continue.
    // this can be used in cases where we don't have to worry about
    // overflow (e.g. Double, or Rational).
    def iter0(start: A, continue: A => Boolean): Iterator[A] =
      new Iterator[A] {
        var x: A = start
        def hasNext: Boolean = continue(x)
        def next: A = {
          val r = x
          x += step
          r
        }
      }

    // build an iterator, using start, step, continue, and test.
    // test is used to detect overflow in cases where it can happen.
    // it won't always be necessary but there isn't currently a typeclass
    // that lets us know when we need to do it.
    def iter1(start: A, continue: A => Boolean, test: (A, A) => Boolean): Iterator[A] =
      new Iterator[A] {
        var x: A = start
        var ok: Boolean = true
        def hasNext: Boolean = ok && continue(x)
        def next: A = {
          val r = x
          val next = x + step
          if (test(x, next)) { x = next } else { ok = false }
          r
        }
      }

    def iter(start: A, safe: Boolean, continue: A => Boolean, test: (A, A) => Boolean): Iterator[A] =
      if (nt.overflows && !safe) {
        iter1(start, continue, test)
      } else {
        iter0(start, continue)
      }

    // build the actual iterator, which primarily relies on figuring
    // out which "direction" we are moving (based on the sign of the
    // step) as well as what kind of limiting bounds we have.
    if (step === ev.zero) {
      throw new IllegalArgumentException("zero step")
    } else if (step > ev.zero) {
      val x = getStart(lowerBound, step, "positive step with no lower bound")
      val test = (x1: A, x2: A) => x1 < x2
      upperBound match {
        case EmptyBound() => Iterator.empty
        case Unbound() => iter(x, false, _ => true, test)
        case Closed(y) => iter(x, y + step > y, _ <= y, test)
        case Open(y) => iter(x, y + step > y, _ < y, test)
      }
    } else {
      val x = getStart(upperBound, step, "negative step with no lower bound")
      val test = (x1: A, x2: A) => x1 > x2
      lowerBound match {
        case EmptyBound() => Iterator.empty
        case Unbound() => iter(x, false, _ => true, test)
        case Closed(y) => iter(x, y + step < y, _ >= y, test)
        case Open(y) => iter(x, y + step < y, _ > y, test)
      }
    }
  }

  def loop(step: A)(f: A => Unit)(implicit o: Order[A], ev: AdditiveMonoid[A], nt: NumberTag[A]): Unit =
    iterator(step).foreach(f)

  def foldOver[B](init: B, step: A)(f: (B, A) => B)(implicit o: Order[A], ev: AdditiveMonoid[A], nt: NumberTag[A]): B =
    iterator(step).foldLeft(init)(f)

  /**
    * Result of overlapping this interval with another one.
    * Can be one of the following:
    * - [[Equal]] if intervals are equal
    * - [[Disjoint]] if intervals are notEmpty don't intersect
    * - [[PartialOverlap]] if intervals intersect and neither is a subset of another
    * - [[Subset]] if one interval (possibly empty) is a subset of another
    *
    * Except for [[Equal]], both original intervals are bound to respective result fields,
    * allowing to determine exact overlap type.
    *
    * For example (pseudo-code):
    * {
    * val a = [5, 6]
    * val b = (0, 1)
    *
    * // this returns Disjoint(b, a). Note a and b placement here, it means that b is strictly less then a.
    * a.overlap(b)
    * }
    */
  def overlap(rhs: Interval[A])(implicit o: Order[A]): Overlap[A] = Overlap(lhs, rhs)
}

case class All[A] private[spire]() extends Interval[A] {
  def lowerBound: Unbound[A] = Unbound()
  def upperBound: Unbound[A] = Unbound()
}

case class Above[A] private[spire](lower: A, flags: Int) extends Interval[A] {
  def lowerBound: ValueBound[A] = if (isOpenLower(flags)) Open(lower) else Closed(lower)
  def upperBound: Unbound[A] = Unbound()
}

case class Below[A] private[spire](upper: A, flags: Int) extends Interval[A] {
  def lowerBound: Unbound[A] = Unbound()
  def upperBound: ValueBound[A] = if (isOpenUpper(flags)) Open(upper) else Closed(upper)
}

// Bounded, non-empty interval with lower < upper
case class Bounded[A] private[spire](lower: A, upper: A, flags: Int) extends Interval[A] {
  def lowerBound: ValueBound[A] = if (isOpenLower(flags)) Open(lower) else Closed(lower)
  def upperBound: ValueBound[A] = if (isOpenUpper(flags)) Open(upper) else Closed(upper)
}

case class Point[A] private[spire](value: A) extends Interval[A] {
  def lowerBound: Closed[A] = Closed(value)
  def upperBound: Closed[A] = Closed(value)
}

case class Empty[A] private[spire]() extends Interval[A] {
  def lowerBound: EmptyBound[A] = EmptyBound()
  def upperBound: EmptyBound[A] = EmptyBound()
}

object Interval {
  import interval._

  private[spire] def withFlags[A: Order](lower: A, upper: A, flags: Int): Interval[A] =
    if (lower < upper)
      Bounded(lower, upper, flags)
    else if (lower === upper && flags == 0)
      Point(lower)
    else
      Interval.empty[A]

  def empty[A](implicit o: Order[A]): Interval[A] = Empty[A]

  def point[A: Order](a: A): Interval[A] = Point(a)

  def zero[A](implicit o: Order[A], r: Semiring[A]): Interval[A] = Point(r.zero)

  def all[A: Order]: Interval[A] = All[A]()

  def apply[A: Order](lower: A, upper: A): Interval[A] = closed(lower, upper)

  /**
   * Return an Interval[Rational] that corresponds to the error bounds
   * for the given Double value.
   *
   * The error bounds are represented as a closed interval, whose
   * lower point is midway between d and the adjacent Double value
   * below it. Similarly, the upper bound is the point midway between
   * d and the adjacent Double value above it.
   *
   * There are three Double values that return "special" intervals:
   *
   *    Infinity => Interval.above(Double.MaxValue)
   *   -Infinity => Interval.below(Double.MinValue)
   *         NaN => Interval.empty
   */
  def errorBounds(d: Double): Interval[Rational] =
    if (d == Double.PositiveInfinity) {
      Interval.above(Double.MaxValue)
    } else if (d == Double.NegativeInfinity) {
      Interval.below(Double.MinValue)
    } else if (isNaN(d)) {
      Interval.empty[Rational]
    } else {
      val n0 = Rational(Math.nextAfter(d, -1.0))
      val n1 = Rational(d)
      val n2 = Rational(Math.nextUp(d))
      Interval((n1 - n0) / 2 + n0, (n2 - n1) / 2 + n1)
    }

  @inline private[spire] final def closedLowerFlags = 0
  @inline private[spire] final def openLowerFlags = 1
  @inline private[spire] final def closedUpperFlags = 0
  @inline private[spire] final def openUpperFlags = 2

  /**
   * Constructs an interval from bounds.
   *
   * This method assumes that lower < upper to avoid comparisons.
   *
   * - When one of the arguments is Unbound, the result will be All,
   *   Above(x, _), or Below(y, _).
   *
   * - When both arguments are Open/Closed (e.g. Open(x), Open(y)),
   *   then x < y and the result will be a Bounded interval.
   *
   * - If both arguments are EmptyBound, the result is Empty.
   *
   * - Any other arguments are invalid.
   *
   * This method cannot construct Point intervals.
   */
  private[spire] def fromOrderedBounds[A: Order](lower: Bound[A], upper: Bound[A]): Interval[A] =
    (lower, upper) match {
      case (EmptyBound(), EmptyBound()) => empty
      case (Closed(x), Closed(y)) => Bounded(x, y, closedLowerFlags | closedUpperFlags)
      case (Open(x), Open(y)) => Bounded(x, y, openLowerFlags | openUpperFlags)
      case (Unbound(), Open(y)) => below(y)
      case (Open(x), Unbound()) => above(x)
      case (Unbound(), Closed(y)) => atOrBelow(y)
      case (Closed(x), Unbound()) => atOrAbove(x)
      case (Closed(x), Open(y)) => Bounded(x, y, closedLowerFlags | openUpperFlags)
      case (Open(x), Closed(y)) => Bounded(x, y, openLowerFlags | closedUpperFlags)
      case (Unbound(), Unbound()) => all
      case (EmptyBound(), _) | (_, EmptyBound()) =>
        throw new IllegalArgumentException("invalid empty bound")
    }

  def fromBounds[A: Order](lower: Bound[A], upper: Bound[A]): Interval[A] =
    (lower, upper) match {
      case (EmptyBound(), EmptyBound()) => empty
      case (Closed(x), Closed(y)) => closed(x, y)
      case (Open(x), Open(y)) => open(x, y)
      case (Unbound(), Open(y)) => below(y)
      case (Open(x), Unbound()) => above(x)
      case (Unbound(), Closed(y)) => atOrBelow(y)
      case (Closed(x), Unbound()) => atOrAbove(x)
      case (Closed(x), Open(y)) => openUpper(x, y)
      case (Open(x), Closed(y)) => openLower(x, y)
      case (Unbound(), Unbound()) => all
      case (EmptyBound(), _) | (_, EmptyBound()) =>
        throw new IllegalArgumentException("invalid empty bound")
    }

  def closed[A: Order](lower: A, upper: A): Interval[A] = {
    val c = lower compare upper
    if (c < 0) Bounded(lower, upper, 0)
    else if (c == 0) Point(lower)
    else Interval.empty[A]
  }
  def open[A: Order](lower: A, upper: A): Interval[A] =
    if (lower < upper) Bounded(lower, upper, 3) else Interval.empty[A]
  def openLower[A: Order](lower: A, upper: A): Interval[A] =
    if (lower < upper) Bounded(lower, upper, 1) else Interval.empty[A]
  def openUpper[A: Order](lower: A, upper: A): Interval[A] =
    if (lower < upper) Bounded(lower, upper, 2) else Interval.empty[A]
  def above[A: Order](a: A): Interval[A] = Above(a, 1)
  def below[A: Order](a: A): Interval[A] = Below(a, 2)
  def atOrAbove[A: Order](a: A): Interval[A] = Above(a, 0)
  def atOrBelow[A: Order](a: A): Interval[A] = Below(a, 0)

  private val NullRe = "^ *\\( *Ø *\\) *$".r
  private val SingleRe = "^ *\\[ *([^,]+) *\\] *$".r
  private val PairRe = "^ *(\\[|\\() *(.+?) *, *(.+?) *(\\]|\\)) *$".r

  def apply(s: String): Interval[Rational] =
    s match {
      case NullRe() => Interval.empty[Rational]
      case SingleRe(x) => Interval.point(Rational(x))
      case PairRe(left, x, y, right) =>
        (left, x, y, right) match {
          case ("(", "-∞", "∞", ")") => Interval.all[Rational]
          case ("(", "-∞", y, ")") => Interval.below(Rational(y))
          case ("(", "-∞", y, "]") => Interval.atOrBelow(Rational(y))
          case ("(", x, "∞", ")") => Interval.above(Rational(x))
          case ("[", x, "∞", ")") => Interval.atOrAbove(Rational(x))
          case ("[", x, y, "]") => Interval.closed(Rational(x), Rational(y))
          case ("(", x, y, ")") => Interval.open(Rational(x), Rational(y))
          case ("[", x, y, ")") => Interval.openUpper(Rational(x), Rational(y))
          case ("(", x, y, "]") => Interval.openLower(Rational(x), Rational(y))
          case _ => throw new NumberFormatException("Impossible: " + s)
        }
      case _ => throw new NumberFormatException("For input string: " + s)
    }

  implicit def eq[A: Eq]: Eq[Interval[A]] =
    new Eq[Interval[A]] {
      def eqv(x: Interval[A], y: Interval[A]): Boolean = x == y
    }

  implicit def semiring[A](implicit ev: Ring[A], o: Order[A]): Semiring[Interval[A]] =
    new Semiring[Interval[A]] {
      def zero: Interval[A] = Interval.point(ev.zero)
      def plus(x: Interval[A], y: Interval[A]): Interval[A] = x + y
      def times(x: Interval[A], y: Interval[A]): Interval[A] = x * y
      override def pow(x: Interval[A], k: Int): Interval[A] = x.pow(k)
    }
}
