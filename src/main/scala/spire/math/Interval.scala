package spire.math

import spire.algebra._
import Implicits._

import language.implicitConversions

/**
 * A Bound represents one side of an interval; it is parameterized on T, the
 * ordered type covered by the interval.
 *
 * Bound does not imply anything about being an upper and lower bound, or
 * necessarily having a value (it may represent a limit like +infinity).
 */
sealed trait Bound[T] {
  implicit def order:Order[T]

  def toUpper:Upper[T]
  def toLower:Lower[T]

  def compare(rhs:Bound[T]):Int
  def comparePt(t:T):Int

  def unop(f:T => T):Bound[T]
  def binop(rhs:Bound[T])(f:(T, T) => T):Bound[T]

  def <(rhs:Bound[T]) = compare(rhs) < 0
  def <=(rhs:Bound[T]) = compare(rhs) < 1
  def >(rhs:Bound[T]) = compare(rhs) > 0
  def >=(rhs:Bound[T]) = compare(rhs) > -1

  def min(rhs:Bound[T]):Bound[T] = if (this < rhs) this else rhs
  def max(rhs:Bound[T]):Bound[T] = if (this > rhs) this else rhs
}

/**
 * This class provides operators for Bound[T] instances when T is a member of
 * the Ring typeclass (i.e. an implicit Ring[T] exists).
 */
class BoundRingOps[T](lhs:Bound[T])(implicit ev:Ring[T]) {
  //def abs = lhs.unop(_.abs)
  def unary_- = lhs.unop(ev.negate(_))
  def +(rhs:T) = lhs.unop(ev.plus(_, rhs))
  def -(rhs:T) = lhs.unop(ev.minus(_, rhs))
  def *(rhs:T) = lhs.unop(ev.times(_, rhs))
  def +(rhs:Bound[T]) = lhs.binop(rhs)(ev.plus(_, _))
  def -(rhs:Bound[T]) = lhs.binop(rhs)(ev.minus(_, _))
  def *(rhs:Bound[T]) = lhs.binop(rhs)(ev.times(_, _))
  def pow(rhs:Int) = lhs.unop(ev.pow(_, rhs))
}

/**
 * This class provides operators for Bound[T] instances when T is a member of
 * the EuclideanRing typeclass (i.e. an implicit EuclideanRing[T] exists).
 */
class BoundEuclideanRingOps[T](lhs:Bound[T])(implicit ev:EuclideanRing[T]) {
  def /~(rhs:T) = lhs.unop(ev.quot(_, rhs))
  def /~(rhs:Bound[T]) = lhs.binop(rhs)(ev.quot(_, _))
}

/**
 * This class provides operators for Bound[T] instances when T is a member of
 * the Field typeclass (i.e. an implicit Field[T] exists).
 */
class BoundFieldOps[T](lhs:Bound[T])(implicit ev:Field[T]) {
  def /(rhs:T) = lhs.unop(ev.div(_, rhs))
  def /(rhs:Bound[T]) = lhs.binop(rhs)(ev.div(_, _))
}

/**
 * Lower represents a Bound which is a lower bound.
 */
sealed trait Lower[T] extends Bound[T] { def toLower = this }

/**
 * Upper represents a Bound which is an upper bound.
 */
sealed trait Upper[T] extends Bound[T] { def toUpper = this }

/**
 * Unbound represents a boundary which does not limit its "side" of the
 * interval, i.e. +/- infinity.
 */
sealed trait Unbound[T] extends Bound[T] {
  def unop(f:T => T):Bound[T] = this
  def binop(rhs:Bound[T])(f:(T, T) => T):Bound[T] = this
}

/**
 * UnboundBelow represents an unrestrainted lower bound, i.e. negative infinity.
 */
case class UnboundBelow[T]()(implicit val order:Order[T]) extends Lower[T] with Unbound[T] {
  def compare(rhs:Bound[T]) = rhs match {
    case UnboundBelow() => 0
    case _ => -1
  }
  def comparePt(t:T) = -1
  def toUpper = UnboundAbove[T]
}

/**
 * UnboundAbove represents an unrestrainted upper bound, i.e. positive infinity.
 */
case class UnboundAbove[T]()(implicit val order:Order[T]) extends Upper[T] with Unbound[T] {
  def compare(rhs:Bound[T]) = rhs match {
    case UnboundAbove() => 0
    case _ => 1
  }
  def comparePt(t:T) = 1
  def toLower = UnboundBelow[T]
}

/**
 * Closed is a closed bound (the interval contains this boundary limit).
 */
sealed trait Closed[T] {
  implicit def order:Order[T]
  def x:T
  def compare(rhs:Bound[T]) = rhs match {
    case UnboundBelow() => 1
    case UnboundAbove() => -1 // claimed to be unreachable???
    case ClosedBelow(y) => order.compare(x, y)
    case ClosedAbove(y) => order.compare(x, y)
    case OpenBelow(y) => if (order.lteqv(x, y)) -1 else 1
    case OpenAbove(y) => if (order.lt(x, y)) -1 else 1
  }
  def comparePt(t:T) = order.compare(x, t)
  def binop(rhs:Bound[T])(f:(T, T) => T):Bound[T] = rhs match {
    case UnboundBelow() => UnboundBelow()
    case UnboundAbove() => UnboundBelow()
    case OpenBelow(y) => OpenBelow(f(x, y))
    case OpenAbove(y) => OpenBelow(f(x, y))
    case ClosedBelow(y) => ClosedBelow(f(x, y))
    case ClosedAbove(y) => ClosedBelow(f(x, y))
  }
}

/**
 * ClosedBelow is a closed lower bound, i.e. a in the interval [a, b].
 */
case class ClosedBelow[T](x:T)(implicit val order:Order[T]) extends Lower[T] with Closed[T] {
  def toUpper = ClosedAbove(x)
  def unop(f:T => T) = ClosedBelow(f(x))
  override def binop(rhs:Bound[T])(f:(T, T) => T):Lower[T] = super.binop(rhs)(f).toLower
}

/**
 * ClosedAbove is a closed upper bound, i.e. b in the interval [a, b]
 */
case class ClosedAbove[T](x:T)(implicit val order:Order[T]) extends Upper[T] with Closed[T] {
  def toLower = ClosedBelow(x)
  def unop(f:T => T) = ClosedAbove(f(x))
  override def binop(rhs:Bound[T])(f:(T, T) => T):Upper[T] = super.binop(rhs)(f).toUpper
}

/**
 * Open is a open bound (the interval does not contain this boundary limit).
 */
sealed trait Open[T] {
  implicit def order:Order[T]
  def x:T
  def binop(rhs:Bound[T])(f:(T, T) => T):Bound[T] = rhs match {
    case UnboundBelow() => UnboundBelow()
    case UnboundAbove() => UnboundBelow()
    case OpenBelow(y) => OpenBelow(f(x, y))
    case OpenAbove(y) => OpenBelow(f(x, y))
    case ClosedBelow(y) => OpenBelow(f(x, y))
    case ClosedAbove(y) => OpenBelow(f(x, y))
  }
}

/**
 * OpenBelow is an open lower bound, i.e. a in the interval (a, b).
 */
case class OpenBelow[T](x:T)(implicit val order:Order[T]) extends Lower[T] with Open[T] {
  def compare(rhs:Bound[T]) = rhs match {
    case UnboundBelow() => 1
    case UnboundAbove() => -1
    case ClosedBelow(y) => if (order.lt(x, y)) -1 else 1
    case ClosedAbove(y) => if (order.lt(x, y)) -1 else 1
    case OpenBelow(y) => order.compare(x, y)
    case OpenAbove(y) => if (order.lt(x, y)) -1 else 1
  }
  def comparePt(t:T) = if (order.lt(x, t)) -1 else 1
  def toUpper = OpenAbove(x)
  def unop(f:T => T) = OpenBelow(f(x))
  override def binop(rhs:Bound[T])(f:(T, T) => T):Lower[T] = super.binop(rhs)(f).toLower
}

/**
 * OpenAbove is an open upper bound, i.e. b in the interval (a, b).
 */
case class OpenAbove[T](x:T)(implicit val order:Order[T]) extends Upper[T] with Open[T] {
  def compare(rhs:Bound[T]) = rhs match {
    case UnboundBelow() => 1
    case UnboundAbove() => -1
    case ClosedBelow(y) => if (order.lteqv(x, y)) -1 else 1
    case ClosedAbove(y) => if (order.lteqv(x, y)) -1 else 1
    case OpenBelow(y) => if (order.lteqv(x, y)) -1 else 1
    case OpenAbove(y) => order.compare(x, y)
  }
  def comparePt(t:T) = if (order.lteqv(x, t)) -1 else 1
  def toLower = OpenBelow(x)
  def unop(f:T => T) = OpenAbove(f(x))
  override def binop(rhs:Bound[T])(f:(T, T) => T):Upper[T] = super.binop(rhs)(f).toUpper
}

/**
 * GenInterval is a trait representing the most generic kind of interval.
 * In this case, the only information we have about T is that it can be
 * ordered... no other mathematical operations are allowed.
 *
 * GenInterval's concrete counterpart is Interval.
 */
trait GenInterval[T, U <: GenInterval[T, U]] {
  implicit def order:Order[T]

  def lower:Lower[T]
  def upper:Upper[T]

  protected[this] def coerce(a:Bound[T], b:Bound[T]):U

  def isAbove(t:T) = 0 < lower.comparePt(t)
  def isBelow(t:T) = upper.comparePt(t) < 0
  def isAt(t:T) = lower.comparePt(t) == 0 && 0 == upper.comparePt(t)
  def contains(t:T) = lower.comparePt(t) <= 0 && 0 <= upper.comparePt(t)
  def crosses(t:T) = lower.comparePt(t) < 0 && 0 < upper.comparePt(t)

  def mask(rhs:U):U = coerce(lower max rhs.lower, upper min rhs.upper)

  def split(t:T):(U, U) = {
    val below = coerce(UnboundBelow[T], OpenAbove(t))
    val above = coerce(OpenBelow(t), UnboundAbove[T])
    (this mask below, this mask above)
  }
}

/**
 * GenRingInterval is an interval whose T is a member of Ring.
 *
 * This Ring[T] enables us to support the Ring operations over intervals, i.e.
 * basic interval arithmetic. Obviously things like division are not included.
 */
trait GenRingInterval[T, U <: GenRingInterval[T, U]] extends GenInterval[T, U] {
  implicit def num:Ring[T]

  implicit def boundRingOps(b:Bound[T]) = new BoundRingOps(b)

  def splitAtZero = split(num.zero)

  /* TODO: Required :Signed[T]?
  def abs = {
    val a = lower.abs
    val b = upper.abs
    if (crosses(num.zero)) coerce(ClosedBelow(num.zero), a max b)
    else if (a < b) coerce(a, b)
    else coerce(b, a)
  }
  */
  
  def unary_- = coerce(-upper, -lower)
  
  def +(rhs:U):U = coerce(lower + rhs.lower, upper + rhs.upper)
  def +(rhs:T):U = coerce(lower + rhs, upper + rhs)
  
  def -(rhs:U):U = coerce(lower - rhs.upper, upper - rhs.lower)
  def -(rhs:T):U = coerce(lower - rhs, upper - rhs)
  
  def *(rhs:U):U = {
    val tcz = crosses(num.zero)
    val rcz = rhs.crosses(num.zero)
  
    val ll = lower * rhs.lower
    val lu = lower * rhs.upper
    val ul = upper * rhs.lower
    val uu = upper * rhs.upper
  
    if (tcz && rcz) {
      coerce(lu min ul, ll max uu)
    } else if (tcz) {
      coerce(ll min lu, ul max uu)
    } else if (rcz) {
      coerce(ll min ul, lu max uu)
    } else if (isBelow(num.zero) == rhs.isBelow(num.zero)) {
      coerce(ll min uu, ll max uu)
    } else {
      coerce(lu min ul, lu max ul)
    }
  }
  def *(rhs:T):U = {
    val a = lower * rhs
    val b = upper * rhs
    if (a < b) coerce(a, b) else coerce(b, a)
  }

  def pow(rhs:Int):U = {
    val a = lower pow rhs
    val b = upper pow rhs
  
    if (contains(num.zero) && rhs % 2 == 0) {
      coerce(ClosedBelow(num.zero), a max b)
    } else {
      if (a < b) coerce(a, b) else coerce(b, a)
    }
  }
}

/**
 * GenDiscreteInterval is an interval whose T is a member of EuclideanRing.
 *
 * In addition to the operations on ring intervals, this interval also
 * supports quotient.
 */
trait GenDiscreteInterval[T, U <: GenDiscreteInterval[T, U]] extends GenRingInterval[T, U] {
  implicit def num:EuclideanRing[T]
  //implicit def order:Order[T]

  implicit def boundEuclideanRingOps(b:Bound[T]) = new BoundEuclideanRingOps(b)

  def /~(rhs:U):U = {
    if (rhs.contains(num.zero)) sys.error("divide-by-zero possible")

    val ll = lower /~ rhs.lower
    val lu = lower /~ rhs.upper
    val ul = upper /~ rhs.lower
    val uu = upper /~ rhs.upper

    val bz = rhs.isBelow(num.zero)

    if (crosses(num.zero)) {
      if (bz) coerce(uu, lu) else coerce(ll, ul)
    } else if (isAbove(num.zero)) {
      if (bz) coerce(uu, ll) else coerce(lu, ul)
    } else {
      if (bz) coerce(ul, lu) else coerce(ll, uu)
    }
  }
  def /~(rhs:T) = {
    val a = lower /~ rhs
    val b = upper /~ rhs
    if (a < b) coerce(a, b) else coerce(b, a)
  }
}

/**
 * GenContinuousInterval is an interval whose T is a member of Field.
 *
 * In addition to the operations on discrete intervals, this interval also
 * supports division.
 */
trait GenContinuousInterval[T, U <: GenContinuousInterval[T, U]] extends GenRingInterval[T, U] {
  implicit def num:Field[T]
  //implicit def order:Order[T]

  implicit def boundFieldOps(b:Bound[T]) = new BoundFieldOps(b)

  def /(rhs:U):U = {
    if (rhs.contains(num.zero)) sys.error("divide-by-zero possible")

    val ll = lower / rhs.lower
    val lu = lower / rhs.upper
    val ul = upper / rhs.lower
    val uu = upper / rhs.upper

    val bz = rhs.isBelow(num.zero)

    if (crosses(num.zero)) {
      if (bz) coerce(uu, lu) else coerce(ll, ul)
    } else if (isAbove(num.zero)) {
      if (bz) coerce(uu, ll) else coerce(lu, ul)
    } else {
      if (bz) coerce(ul, lu) else coerce(ll, uu)
    }
  }
  def /(rhs:T) = {
    val a = lower / rhs
    val b = upper / rhs
    if (a < b) coerce(a, b) else coerce(b, a)
  }
}

/**
 * Interval represents a lower and upper bound of an ordered type T.
 *
 * This interval may be unbounded (i.e. (-inf, inf)), half-bounded
 * (i.e. (-inf, 3)), or bounded (i.e. (-3, 3)).
 */
case class Interval[T](lower:Lower[T], upper:Upper[T])(implicit val order:Order[T])
extends GenInterval[T, Interval[T]] {
  protected[this] def coerce(a:Bound[T], b:Bound[T]) = Interval(a.toLower, b.toUpper)
}

/**
 * RingInterval represents a lower and upper bound of an ordered type T, where
 * T is a member of Ring. RingIntervals can be combined using the interval
 * arithmetic operations defined in Ring.
 */
case class RingInterval[T](lower:Lower[T], upper:Upper[T])(implicit val order:Order[T], val num:Ring[T])
extends GenRingInterval[T, RingInterval[T]] {
  protected[this] def coerce(a:Bound[T], b:Bound[T]) = RingInterval(a.toLower, b.toUpper)
}

/**
 * DiscreteInterval represents a lower and upper bound of an ordered type T,
 * where T is a member of EuclideanRing. DiscreteIntervals support all ring
 * operations as well as quotient.
 */
case class DiscreteInterval[T](lower:Lower[T], upper:Upper[T])(implicit ev:Integral[T])
extends GenDiscreteInterval[T, DiscreteInterval[T]] {
  implicit def num:EuclideanRing[T] = ev
  implicit def order:Order[T] = ev
  protected[this] def coerce(a:Bound[T], b:Bound[T]) = DiscreteInterval(a.toLower, b.toUpper)
}

/**
 * ContinuousInterval represents a lower and upper bound of an ordered type T,
 * where T is a member of EuclideanRing. ContinuousIntervals supports all ring
 * operations as well as quotient and division.
 */
case class ContinuousInterval[T](lower:Lower[T], upper:Upper[T])(implicit ev:Fractional[T])
extends GenContinuousInterval[T, ContinuousInterval[T]] {
  implicit def num:Field[T] = ev
  implicit def order:Order[T] = ev
  protected[this] def coerce(a:Bound[T], b:Bound[T]) = ContinuousInterval(a.toLower, b.toUpper)
}
