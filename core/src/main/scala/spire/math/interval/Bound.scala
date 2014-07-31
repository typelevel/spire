package spire.math
package interval

import spire.syntax.order._
import spire.syntax.field._
import spire.syntax.nroot._
import spire.algebra._

sealed trait Bound[A] { lhs =>
  def map[B](f: A => B): Bound[B] = this match {
    case Open(a) => Open(f(a))
    case Closed(a) => Closed(f(a))
    case Unbound() => Unbound()
    case EmptyBound() => EmptyBound()
  }
  def combine[B](rhs: Bound[A])(f: (A, A) => A): Bound[A] = (lhs, rhs) match {
    case (EmptyBound(), _) => lhs
    case (_, EmptyBound()) => rhs
    case (Unbound(), _) => lhs
    case (_, Unbound()) => rhs
    case (Closed(a), y) => y.map(b => f(a, b))
    case (x, Closed(b)) => x.map(a => f(a, b))
    case (Open(a), Open(b)) => Open(f(a, b))
  }

  def unary_-()(implicit ev: AdditiveGroup[A]): Bound[A] =
    lhs.map(-_)
  def reciprocal()(implicit ev: MultiplicativeGroup[A]): Bound[A] =
    lhs.map(_.reciprocal)

  def +(a: A)(implicit ev: AdditiveSemigroup[A]): Bound[A] = map(_ + a)
  def -(a: A)(implicit ev: AdditiveGroup[A]): Bound[A] = map(_ - a)
  def *(a: A)(implicit ev: MultiplicativeSemigroup[A]): Bound[A] = map(_ * a)
  def /(a: A)(implicit ev: MultiplicativeGroup[A]): Bound[A] = map(_ / a)

  def +(rhs: Bound[A])(implicit ev: AdditiveSemigroup[A]): Bound[A] =
    lhs.combine(rhs)(_ + _)
  def -(rhs: Bound[A])(implicit ev: AdditiveGroup[A]): Bound[A] =
    lhs.combine(rhs)(_ - _)
  def *(rhs: Bound[A])(implicit ev: MultiplicativeSemigroup[A]): Bound[A] =
    lhs.combine(rhs)(_ * _)
  def /(rhs: Bound[A])(implicit ev: MultiplicativeGroup[A]): Bound[A] =
    lhs.combine(rhs)(_ / _)
}

object Bound {
  private[spire] def minLower[A: Order](lhs: Bound[A], rhs: Bound[A], emptyIsMin: Boolean): Bound[A] =
    (lhs, rhs) match {
      case (EmptyBound(), _) => if (emptyIsMin) lhs else rhs
      case (_, EmptyBound()) => if (emptyIsMin) rhs else lhs
      case (Unbound(), _) | (_, Unbound()) => Unbound()
      case (Closed(lv), Closed(rv)) if lv <= rv => lhs
      case (Closed(_), Closed(_)) => rhs
      case (Open(lv), Open(rv)) if lv <= rv => lhs
      case (Open(_), Open(_)) => rhs
      case (Closed(lv), Open(rv)) if lv <= rv => lhs
      case (Closed(_), Open(_)) => rhs
      case (Open(lv), Closed(rv)) if rv <= lv => rhs
      case (Open(_), Closed(_)) => lhs
    }

  private[spire] def maxLower[A: Order](lhs: Bound[A], rhs: Bound[A], emptyIsMax: Boolean): Bound[A] =
    (lhs, rhs) match {
      case (EmptyBound(), _) => if (emptyIsMax) lhs else rhs
      case (_, EmptyBound()) => if (emptyIsMax) rhs else lhs
      case (Unbound(), _) => rhs
      case (_, Unbound()) => lhs
      case (Closed(lv), Closed(rv)) if lv >= rv => lhs
      case (Closed(_), Closed(_)) => rhs
      case (Open(lv), Open(rv)) if lv >= rv => lhs
      case (Open(_), Open(_)) => rhs
      case (Closed(lv), Open(rv)) if rv >= lv => rhs
      case (Closed(_), Open(_)) => lhs
      case (Open(lv), Closed(rv)) if lv >= rv => lhs
      case (Open(_), Closed(_)) => rhs
    }

  private[spire] def minUpper[A: Order](lhs: Bound[A], rhs: Bound[A], emptyIsMin: Boolean): Bound[A] =
    (lhs, rhs) match {
      case (EmptyBound(), _) => if (emptyIsMin) lhs else rhs
      case (_, EmptyBound()) => if (emptyIsMin) rhs else lhs
      case (Unbound(), _) => rhs
      case (_, Unbound()) => lhs
      case (Closed(lv), Closed(rv)) if lv <= rv => lhs
      case (Closed(_), Closed(_)) => rhs
      case (Open(lv), Open(rv)) if lv <= rv => lhs
      case (Open(_), Open(_)) => rhs
      case (Closed(lv), Open(rv)) if rv <= lv => rhs
      case (Closed(_), Open(_)) => lhs
      case (Open(lv), Closed(rv)) if lv <= rv => lhs
      case (Open(_), Closed(_)) => rhs
    }

  private[spire] def maxUpper[A: Order](lhs: Bound[A], rhs: Bound[A], emptyIsMax: Boolean): Bound[A] =
    (lhs, rhs) match {
      case (EmptyBound(), _) => if (emptyIsMax) lhs else rhs
      case (_, EmptyBound()) => if (emptyIsMax) rhs else lhs
      case (Unbound(), _) | (_, Unbound()) => Unbound()
      case (Closed(lv), Closed(rv)) if lv >= rv => lhs
      case (Closed(_), Closed(_)) => rhs
      case (Open(lv), Open(rv)) if lv >= rv => lhs
      case (Open(_), Open(_)) => rhs
      case (Closed(lv), Open(rv)) if lv >= rv => lhs
      case (Closed(_), Open(_)) => rhs
      case (Open(lv), Closed(rv)) if rv >= lv => rhs
      case (Open(_), Closed(_)) => lhs
    }
}

case class EmptyBound[A]() extends Bound[A]

case class Unbound[A]() extends Bound[A]

sealed trait ValueBound[A] extends Bound[A] {
  def a: A
  def isClosed: Boolean
}

case class Open[A](a: A) extends ValueBound[A] {
  def isClosed = false
}

case class Closed[A](a: A) extends ValueBound[A] {
  def isClosed = true
}

/** Companion object for open/closed bounds, used to construct intervals from
  * a set of bounds.
  * 
  * In the comments, we write
  * 
  * - [v, (v or ?v when the bound is interpreted as a lower bound,
  * - v], v), v? when the bound is interpreted as an upper bound.
  * 
  * The symbols [], (), ? correspond to closed, open or unknown bounds.
  */
object ValueBound {
  def unapply[A](b: Bound[A]): Option[A] = b match {
    case Open(a) => Some(a)
    case Closed(a) => Some(a)
    case _ => None
  }

  /** Returns the interval containing the two given bounds. */
  @inline def union2[A: Order](v1: ValueBound[A], v2: ValueBound[A]): Interval[A] =
    v1.a.compare(v2.a).signum match {
      case -1 => // ?v1 < v2?
        Interval.fromOrderedBounds(v1, v2)
      case 1 => // ?v2 < v1?
        Interval.fromOrderedBounds(v2, v1)
      case 0 if v1.isClosed || v2.isClosed => // v1 ~ v2, including a closed bound
        Interval.point(v1.a)
      case 0 => // both bounds are Open(x)
        Interval.empty[A]
    }

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a === v2.a < v3.a.
    */
  @inline def union3_1approx2_2less3[A: Order](v1: ValueBound[A], 
    v2: ValueBound[A], v3: ValueBound[A]): Interval[A] =
    if (v1.isClosed) // [v1 ~ ?v2 < v3?
      Interval.fromOrderedBounds(v1, v3)
    else // (v1 ~ ?v2 < v3?
      Interval.fromOrderedBounds(v2, v3)

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a < v2.a === v3.a.
    */
  @inline def union3_1less2_2approx3[A: Order](v1: ValueBound[A], 
    v2: ValueBound[A], v3: ValueBound[A]): Interval[A] =
    if (v2.isClosed) // ?v1 < v2] ~ v3?
      Interval.fromOrderedBounds(v1, v2)
    else // ?v1 < v2) ~ v3?
      Interval.fromOrderedBounds(v1, v3)

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a === v2.a === v3.a.
    */
  @inline def union3_1approx2_2approx3[A: Order](v1: ValueBound[A],
    v2: ValueBound[A], v3: ValueBound[A]): Interval[A] =
    if (v1.isClosed || v2.isClosed || v3.isClosed)
      Interval.point(v1.a)
    else
      Interval.empty[A]

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a === v2.a.
    */
  @inline def union3_1approx2[A: Order](v1: ValueBound[A], 
    v2: ValueBound[A], v3: ValueBound[A]): Interval[A] =
    v2.a.compare(v3.a).signum match {
      case -1 => // v1 ~ v2 < v3
        union3_1approx2_2less3(v1, v2, v3)
      case 0 => // v1 ~ v2 ~ v3
        union3_1approx2_2approx3(v1, v2, v3)
      case 1 => // v3 < v1 ~ v2
        union3_1less2_2approx3(v3, v1, v2)
    }

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a < v2.a.
    */
  @inline def union3_1less2[A: Order](v1: ValueBound[A],
    v2: ValueBound[A], v3: ValueBound[A]): Interval[A] = 
    v2.a.compare(v3.a).signum match {
      case -1 => // v1 < v2 < v3
        Interval.fromOrderedBounds(v1, v3)
      case 1 => // v1 < v2 and v3 < v2
        v1.a.compare(v3.a).signum match {
          case -1 => // v1 < v3 < v2
            Interval.fromOrderedBounds(v1, v2)
          case 1 => // v3 < v1 < v2
            Interval.fromOrderedBounds(v3, v2)
          case 0 => // v1 ~ v3 < v2
            union3_1approx2_2less3(v1, v3, v2)
        }
      case 0 => // v1 < v2 ~ v3
        union3_1less2_2approx3(v1, v2, v3)
    }

  /** Returns the interval which contains all the given bounds. */
  @inline def union3[A: Order](v1: ValueBound[A], 
    v2: ValueBound[A], v3: ValueBound[A]): Interval[A] =
    v1.a.compare(v2.a).signum match {
      case -1 => // v1 < v2
        union3_1less2(v1, v2, v3)
      case 1 => // v1 > v2
        union3_1less2(v2, v1, v3)
      case 0 => // v1 ~ v2
        union3_1approx2(v1, v2, v3)
    }

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a === v2.a === v3.a < v4.a.
    */
  @inline def union4_1approx2_2approx3_3less4[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    if (v1.isClosed) // [v1 ~ ?v2 ~ ?v3 < v4
      Interval.fromOrderedBounds(v1, v4)
    else if (v2.isClosed) // (v1 ~ [v2 ~ ?v3 < v4
      Interval.fromOrderedBounds(v2, v4)
    else // (v1 ~ (v2 ~ ?v3 < v4
      Interval.fromOrderedBounds(v3, v4)

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a === v2.a < v3.a === v4.a.
    */
  @inline def union4_1approx2_2less3_3approx4[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    (v1.isClosed, v3.isClosed) match {
      case (true, true) => //  [v1 ~ ?v2  <  v3] ~ v4?
        Interval.fromOrderedBounds(v1, v3)
      case (false, true) => //  (v1 ~ ?v2 <  v3] ~ v4?
        Interval.fromOrderedBounds(v2, v3)
      case (true, false) => //  [v1 ~ ?v2 <  v3) ~ v4?
        Interval.fromOrderedBounds(v1, v4)
      case (false, false) => // (v1 ~ ?v2 <  v3) ~ v4?
        Interval.fromOrderedBounds(v2, v4)
    }

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a < v2.a === v3.a === v4.a.
    */
  @inline def union4_1less2_2approx3_3approx4[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    if (v2.isClosed) // ?v1 < v2] ~ v3? ~ v4?
      Interval.fromOrderedBounds(v1, v2)
    else if (v3.isClosed) // ?v1 < v2) ~ v3] ~ v4?
      Interval.fromOrderedBounds(v1, v3)
    else // ?v1 < v2) ~ v3) ~ v4?
      Interval.fromOrderedBounds(v1, v4)


  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a < v3.a === v4.a and v2.a < v3.a === v4.a.
    */
  @inline def union4_1less3_2less3_3approx4[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    v1.a.compare(v2.a).signum match {
      case -1 => // v1 < v2 < v3 ~ v4
        union3_1less2_2approx3(v1, v3, v4)
      case 0 => // v1 ~ v2 < v3 ~ v4
        union4_1approx2_2less3_3approx4(v1, v2, v3, v4)
      case 1 => // v2 < v1 < v3 ~ v4
        union3_1less2_2approx3(v2, v3, v4)
    }

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a < v2.a === v3.a.
    */
  @inline def union4_1less2_2approx3[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    v3.a.compare(v4.a).signum match {
      case -1 => // v1 < v2 ~ v3 < v4
        Interval.fromOrderedBounds(v1, v4)
      case 0 => // v1 < v2 ~ v3 ~ v4
        union4_1less2_2approx3_3approx4(v1, v2, v3, v4)
      case 1 => // v1 < v2 ~ v3 and v4 < v2 ~ v3
        union4_1less3_2less3_3approx4(v1, v4, v2, v3)
    }

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a === v2.a < v3.a.
    */
  @inline def union4_1approx2_2less3[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    v3.a.compare(v4.a).signum match {
      case -1 => // v1 ~ v2 < v3 < v4
        union3_1approx2_2less3(v1, v2, v4)
      case 0 => // v1 ~ v2 < v3 ~ v4
        union4_1approx2_2less3_3approx4(v1, v2, v3, v4)
      case 1 => // v1 ~ v2 < v3 and v4 < v3
        v1.a.compare(v4.a).signum match {
          case -1 => // v1 ~ v2 < v4 < v3
            union3_1approx2_2less3(v1, v2, v3)
          case 0 => // v1 ~ v2 ~ v4 < v3
            union4_1approx2_2approx3_3less4(v1, v2, v4, v3)
          case 1 => // v4 < v1 ~ v2 < v3
            Interval.fromOrderedBounds(v4, v3)
        }
    }

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a === v2.a == v3.a.
    */
  @inline def union4_1approx2_2approx3[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    if (v1.isClosed == v2.isClosed) // [v1] ~ [v2] ~ ?v3? or (v1) ~ (v2) ~ ?v3?
      union3_1approx2(v2, v3, v4)
    else // [v1] ~ (v2) ~ ?v3? or (v1) ~ [v2] ~ ?v3?
      union3_1approx2(v1, v2, v4)

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a === v2.a.
    */
  @inline def union4_1approx2[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    v2.a.compare(v3.a).signum match {
      case -1 => // v1 ~ v2 < v3
        union4_1approx2_2less3(v1, v2, v3, v4)
      case 0 => // v1 ~ v2 ~ v3
        union4_1approx2_2approx3(v1, v2, v3, v4)
      case 1 => // v3 < v1 ~ v2
        union4_1less2_2approx3(v3, v1, v2, v4)
    }

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a < v2.a and v1.a < v3.a (i.e. v1 is the minimum of all).
    */
  @inline def union4_1less2_1less3[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    v2.a.compare(v3.a).signum match {
      case -1 => // v1 < v2 < v3
        union3_1less2(v1, v3, v4)
      case 0 => // v1 < v2 ~ v3
        union4_1less2_2approx3(v1, v2, v3, v4)
      case 1 => // v1 < v3 < v2
        union3_1less2(v1, v2, v4)
    }

  /** Returns the interval which contains all the given bounds.
    * 
    * Assumption: v1.a < v3.a and v2.a < v3.a (i.e. v3 is the maximum of all).
    */
  @inline def union4_1less3_2less3[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    v1.a.compare(v2.a).signum match {
      case -1 => // v1 < v2 < v3
        union3_1less2(v1, v3, v4)
      case 0 => // v1 ~ v2 < v3
        union4_1approx2_2less3(v1, v2, v3, v4)
      case 1 => // v2 < v1 < v3
        union3_1less2(v2, v3, v4)
    }

  /** Returns the interval which contains all the given bounds.
    * *
    * Assumption: v1.a < v2.a.
    */
  @inline def union4_1less2[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    v2.a.compare(v3.a).signum match {
      case -1 => // v1 < v2 < v3
        union3_1less2(v1, v3, v4)
      case 0 => // v1 < v2 ~ v3
        union4_1less2_2approx3(v1, v2, v3, v4)
      case 1 => // v1 < v2 and v3 < v2
        union4_1less3_2less3(v1, v3, v2, v4)
    }

  /** Returns the interval which contains all the given bounds. */
  @inline def union4[A: Order](v1: ValueBound[A], v2: ValueBound[A],
    v3: ValueBound[A], v4: ValueBound[A]): Interval[A] =
    v1.a.compare(v2.a).signum match {
      case -1 => // v1 < v2
        union4_1less2(v1, v2, v3, v4)
      case 1 => // v1 > v2
        union4_1less2(v2, v1, v3, v4)
      case 0 => // v1 ~ v2
        union4_1approx2(v1, v2, v3, v4)
    }
}
