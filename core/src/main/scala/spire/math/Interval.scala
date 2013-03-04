package spire.math

import spire.algebra._

/**
 * A Bound represents one side of an interval; it is parameterized on
 * T, the ordered type covered by the interval.
 *
 * Bound (by itself) does not imply anything about being an upper and
 * lower bound, or necessarily having a value (it may represent a
 * limit like +infinity).
 */
sealed trait Bound[T] {
  // bounds require T to have an Order[T].
  implicit def order: Order[T]

  // convert this bound to an upper/lower bound
  def toUpper: Upper[T]
  def toLower: Lower[T]

  // compare to other bounds/points
  def compare(rhs: Bound[T]): Int
  def comparePt(t: T): Int

  // compare bounds
  def <(rhs: Bound[T]) = compare(rhs) < 0
  def <=(rhs: Bound[T]) = compare(rhs) < 1
  def >(rhs: Bound[T]) = compare(rhs) > 0
  def >=(rhs: Bound[T]) = compare(rhs) > -1

  def min(rhs: Bound[T]): Bound[T] = if (compare(rhs) < 0) this else rhs
  def max(rhs: Bound[T]): Bound[T] = if (compare(rhs) > 0) this else rhs

  // perform unary ops (correctly handling open/closed, upper/lower)
  def unop(f: T => T): Bound[T]

  // perform binary ops (correctly handling open/closed, upper/lower)
  def binop(rhs: Bound[T])(f: (T, T) => T): Bound[T]

  // TODO: it would be nice to avoid the overhead of unop/binop, but
  // for now the amount of boilerplate required to accomplish that is
  // simply too high. we could probably solve this with a macro...

  // basic Ring operations involving bounds
  def unary_-(implicit ev: Ring[T]) = unop(ev.negate)
  def +(rhs: T)(implicit ev: Ring[T]) = unop(ev.plus(_, rhs))
  def -(rhs: T)(implicit ev: Ring[T]) = unop(ev.minus(_, rhs))
  def *(rhs: T)(implicit ev: Ring[T]) = unop(ev.times(_, rhs))
  def +(rhs: Bound[T])(implicit ev: Ring[T]) = binop(rhs)(ev.plus)
  def -(rhs: Bound[T])(implicit ev: Ring[T]) = binop(rhs)(ev.minus)
  def *(rhs: Bound[T])(implicit ev: Ring[T]) = binop(rhs)(ev.times)
  def pow(rhs: Int)(implicit ev: Ring[T]) = unop(ev.pow(_, rhs))

  // basic EuclideanRing operations involving bounds
  def /~(rhs: T)(implicit ev: EuclideanRing[T]) = unop(ev.quot(_, rhs))
  def /~:(lhs: T)(implicit ev: EuclideanRing[T]) = unop(ev.quot(lhs, _))
  def /~(rhs: Bound[T])(implicit ev: EuclideanRing[T]) = binop(rhs)(ev.quot)

  // basic Field operations involving bounds
  def /(rhs: T)(implicit ev: Field[T]) = unop(ev.div(_, rhs))
  def /:(lhs: T)(implicit ev: Field[T]) = unop(ev.div(lhs, _))
  def /(rhs: Bound[T])(implicit ev: Field[T]) = binop(rhs)(ev.div)
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
 * Unbound represents a boundary which does not limit its "side" of
 * the interval, i.e. +/- infinity.
 */
sealed trait Unbound[T] extends Bound[T] {
  def unop(f: T => T): Bound[T] = this
  def binop(rhs: Bound[T])(f: (T, T) => T): Bound[T] = this
}

/**
 * UnboundBelow represents an unrestrainted lower bound (i.e. -inf).
 */
final case class UnboundBelow[T]()(implicit val order: Order[T]) extends Lower[T] with Unbound[T] {
  def compare(rhs: Bound[T]) = if (rhs == this) 0 else -1
  def comparePt(t: T) = -1
  def toUpper = UnboundAbove[T]()
}

/**
 * UnboundAbove represents an unrestrainted upper bound (i.e. +inf).
 */
final case class UnboundAbove[T]()(implicit val order: Order[T]) extends Upper[T] with Unbound[T] {
  def compare(rhs: Bound[T]) = if (rhs == this) 0 else 1
  def comparePt(t: T) = 1
  def toLower = UnboundBelow[T]()
}

/**
 * Closed is a closed bound (the interval contains this limit).
 */
sealed trait Closed[T] {
  implicit def order: Order[T]
  def x: T
  def compare(rhs: Bound[T]) = rhs match {
    case UnboundBelow() => 1
    case UnboundAbove() => -1
    case ClosedBelow(y) => order.compare(x, y)
    case ClosedAbove(y) => order.compare(x, y)
    case OpenBelow(y) => if (order.lteqv(x, y)) -1 else 1
    case OpenAbove(y) => if (order.lt(x, y)) -1 else 1
  }
  def comparePt(t: T) = order.compare(x, t)
  def binop(rhs: Bound[T])(f: (T, T) => T): Bound[T] = rhs match {
    case UnboundBelow() => UnboundBelow()
    case UnboundAbove() => UnboundBelow()
    case OpenBelow(y) => OpenBelow(f(x, y))
    case OpenAbove(y) => OpenBelow(f(x, y))
    case ClosedBelow(y) => ClosedBelow(f(x, y))
    case ClosedAbove(y) => ClosedBelow(f(x, y))
  }
}

/**
 * ClosedBelow is a closed lower bound, e.g. a in the interval [a, b].
 */
final case class ClosedBelow[T](x: T)(implicit val order: Order[T]) extends Lower[T] with Closed[T] {
  def toUpper = ClosedAbove(x)
  def unop(f: T => T) = ClosedBelow(f(x))
  override def binop(rhs: Bound[T])(f: (T, T) => T): Lower[T] =
    super.binop(rhs)(f).toLower
}

/**
 * ClosedAbove is a closed upper bound, e.g. b in the interval [a, b]
 */
final case class ClosedAbove[T](x: T)(implicit val order: Order[T]) extends Upper[T] with Closed[T] {
  def toLower = ClosedBelow(x)
  def unop(f: T => T) = ClosedAbove(f(x))
  override def binop(rhs: Bound[T])(f: (T, T) => T): Upper[T] =
    super.binop(rhs)(f).toUpper
}

/**
 * Open is a open bound (the interval does not contain this limit).
 */
sealed trait Open[T] {
  implicit def order: Order[T]
  def x: T
  def binop(rhs: Bound[T])(f: (T, T) => T): Bound[T] = rhs match {
    case UnboundBelow() => UnboundBelow()
    case UnboundAbove() => UnboundBelow()
    case OpenBelow(y) => OpenBelow(f(x, y))
    case OpenAbove(y) => OpenBelow(f(x, y))
    case ClosedBelow(y) => OpenBelow(f(x, y))
    case ClosedAbove(y) => OpenBelow(f(x, y))
  }
}

/**
 * OpenBelow is an open lower bound, e.g. a in the interval (a, b).
 */
final case class OpenBelow[T](x: T)(implicit val order: Order[T]) extends Lower[T] with Open[T] {
  def compare(rhs: Bound[T]) = rhs match {
    case UnboundBelow() => 1
    case UnboundAbove() => -1
    case ClosedBelow(y) => comparePt(y)
    case ClosedAbove(y) => comparePt(y)
    case OpenBelow(y) => order.compare(x, y)
    case OpenAbove(y) => comparePt(y)
  }
  def comparePt(t: T) = if (order.lt(x, t)) -1 else 1
  def toUpper = OpenAbove(x)
  def unop(f: T => T) = OpenBelow(f(x))
  override def binop(rhs: Bound[T])(f: (T, T) => T): Lower[T] =
    super.binop(rhs)(f).toLower
}

/**
 * OpenAbove is an open upper bound, e.g. b in the interval (a, b).
 */
final case class OpenAbove[T](x: T)(implicit val order: Order[T]) extends Upper[T] with Open[T] {
  def compare(rhs: Bound[T]) = rhs match {
    case UnboundBelow() => 1
    case UnboundAbove() => -1
    case ClosedBelow(y) => comparePt(y)
    case ClosedAbove(y) => comparePt(y)
    case OpenBelow(y) => comparePt(y)
    case OpenAbove(y) => order.compare(x, y)
  }
  def comparePt(t: T) = if (order.lteqv(x, t)) -1 else 1
  def toLower = OpenBelow(x)
  def unop(f: T => T) = OpenAbove(f(x))
  override def binop(rhs: Bound[T])(f: (T, T) => T): Upper[T] =
    super.binop(rhs)(f).toUpper
}

/**
 * Interval represents a set of values, usually numbers.
 * 
 * Intervals have upper and lower bounds. Each bound can be one of
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
case class Interval[T](lower: Lower[T], upper: Upper[T])(implicit order: Order[T]) {

  // used to build new intervals with correct lower/upper bounds
  @inline private[math] final def coerce(a: Bound[T], b: Bound[T]) =
    Interval(a.toLower, b.toUpper)

  def isEmpty: Boolean = this match {
    case Interval(OpenBelow(a), OpenAbove(b)) => order.eqv(a, b)
    case _ => false
  }

  def isAbove(t: T) = lower.comparePt(t) > 0
  def isBelow(t: T) = upper.comparePt(t) < 0
  def isAtOrAbove(t: T) = lower.comparePt(t) >= 0
  def isAtOrBelow(t: T) = upper.comparePt(t) <= 0
  def isAt(t: T) = lower.comparePt(t) == 0 && 0 == upper.comparePt(t)
  def contains(t: T) = lower.comparePt(t) <= 0 && 0 <= upper.comparePt(t)
  def crosses(t: T) = lower.comparePt(t) < 0 && 0 < upper.comparePt(t)

  def mask(rhs: Interval[T]): Interval[T] =
    coerce(lower max rhs.lower, upper min rhs.upper)

  def split(t: T): (Interval[T], Interval[T]) = {
    val below = Interval(UnboundBelow[T], OpenAbove(t))
    val above = Interval(OpenBelow(t), UnboundAbove[T])
    (this mask below, this mask above)
  }

  def splitAtZero(implicit ev: Ring[T]) = split(ev.zero)
  
  def unary_-(implicit ev: Ring[T]) = coerce(-upper, -lower)
  
  def +(rhs: Interval[T])(implicit ev: Ring[T]): Interval[T] =
    coerce(lower + rhs.lower, upper + rhs.upper)
  def +(rhs: T)(implicit ev: Ring[T]): Interval[T] =
    coerce(lower + rhs, upper + rhs)
  
  def -(rhs: Interval[T])(implicit ev: Ring[T]): Interval[T] =
    coerce(lower - rhs.upper, upper - rhs.lower)
  def -(rhs: T)(implicit ev: Ring[T]): Interval[T] =
    coerce(lower - rhs, upper - rhs)
  
  def *(rhs: Interval[T])(implicit ev: Ring[T]): Interval[T] = {
    if (this.isEmpty) return this
    if (rhs.isEmpty) return rhs
    if (this.isAt(ev.zero)) return this
    if (rhs.isAt(ev.zero)) return rhs

    val lcz = crosses(ev.zero)
    val rcz = rhs.crosses(ev.zero)
  
    val ll = lower * rhs.lower
    val lu = lower * rhs.upper
    val ul = upper * rhs.lower
    val uu = upper * rhs.upper
  
    if (lcz && rcz)
      coerce(lu min ul, ll max uu)
    else if (lcz)
      coerce(ll min lu, ul max uu)
    else if (rcz)
      coerce(ll min ul, lu max uu)
    else if (isAtOrBelow(ev.zero) == rhs.isAtOrBelow(ev.zero))
      coerce(ll min uu, ll max uu)
    else
      coerce(lu min ul, lu max ul)
  }
  def *(rhs: T)(implicit ev: Ring[T]): Interval[T] = {
    if (order.eqv(rhs, ev.zero)) return Interval.point(ev.zero)
    val a = lower * rhs
    val b = upper * rhs
    if (a < b) coerce(a, b) else coerce(b, a)
  }

  def pow(rhs: Int)(implicit ev: Ring[T]): Interval[T] = {
    val a = lower pow rhs
    val b = upper pow rhs
  
    if (contains(ev.zero) && rhs % 2 == 0)
      coerce(ClosedBelow(ev.zero), a max b)
    else if (a < b)
      coerce(a, b)
    else
      coerce(b, a)
  }

  def mapAroundZero[A](f: Interval[T] => A)(implicit ev: Ring[T]): (A, A) =
    splitAtZero match {
      case (a, b) => (f(a), f(b))
    }

  def /~(rhs: Interval[T])(implicit ev: EuclideanRing[T]): Interval[T] = {
    if (rhs.contains(ev.zero))
      throw new java.lang.ArithmeticException("/ by zero")

    val ll = lower /~ rhs.lower
    val lu = lower /~ rhs.upper
    val ul = upper /~ rhs.lower
    val uu = upper /~ rhs.upper

    val bz = rhs.isBelow(ev.zero)

    if (crosses(ev.zero))
      if (bz) coerce(uu, lu) else coerce(ll, ul)
    else if (isAbove(ev.zero))
      if (bz) coerce(uu, ll) else coerce(lu, ul)
    else
      if (bz) coerce(ul, lu) else coerce(ll, uu)
  }
  def /~(rhs: T)(implicit ev: EuclideanRing[T]): Interval[T] = {
    val a = lower /~ rhs
    val b = upper /~ rhs
    if (a < b) coerce(a, b) else coerce(b, a)
  }
  def /~:(lhs: T)(implicit ev: EuclideanRing[T]): Interval[T] = {
    val a = lhs /~: lower
    val b = lhs /~: upper
    if (a < b) coerce(a, b) else coerce(b, a)
  }

  def /(rhs: Interval[T])(implicit ev: Field[T]): Interval[T] = {
    if (rhs.contains(ev.zero))
      throw new java.lang.ArithmeticException("/ by zero")

    val ll = lower / rhs.lower
    val lu = lower / rhs.upper
    val ul = upper / rhs.lower
    val uu = upper / rhs.upper

    val bz = rhs.isBelow(ev.zero)

    if (crosses(ev.zero))
      if (bz) coerce(uu, lu) else coerce(ll, ul)
    else if (isAbove(ev.zero))
      if (bz) coerce(uu, ll) else coerce(lu, ul)
    else
      if (bz) coerce(ul, lu) else coerce(ll, uu)
  }
  def /(rhs: T)(implicit ev: Field[T]): Interval[T] = {
    val a = lower / rhs
    val b = upper / rhs
    if (a < b) coerce(a, b) else coerce(b, a)
  }
  def /:(lhs: T)(implicit ev: Field[T]): Interval[T] = {
    val a = lhs /: lower
    val b = lhs /: upper
    if (a < b) coerce(a, b) else coerce(b, a)
  }

  override def toString(): String = {
    val a = lower match {
      case ClosedBelow(n) => "[" + n.toString
      case OpenBelow(n) => "(" + n.toString
      case UnboundBelow() => "(-inf"
    }
    val b = upper match {
      case ClosedAbove(n) => n.toString + "]"
      case OpenAbove(n) => n.toString + ")"
      case UnboundAbove() => "inf)"
    }
    a + ", " + b
  }
}

object Interval extends IntervalInstances {

  /**
   * Interval from a to b. By default is this closed, [a, b].
   */
  def apply[T: Order](a: T, b: T): Interval[T] = closed(a, b)

  /**
   * Closed interval [a, b].
   */
  def closed[T: Order](a: T, b: T) = Interval[T](ClosedBelow[T](a), ClosedAbove[T](b))

  /**
   * Open interval (a, b).
   */
  def open[T: Order](a: T, b: T) = Interval[T](OpenBelow[T](a), OpenAbove[T](b))

  /**
   * An interval containing all T values.
   */
  def unbound[T: Order] = Interval(UnboundBelow(), UnboundAbove())

  /**
   * An interval containing exactly one value (a).
   */
  implicit def point[T: Order](a: T) = Interval(ClosedBelow(a), ClosedAbove(a))

  /**
   * An empty interval.
   * 
   * NOTE: Currently we require a Ring[T] to provide a zero
   * value. This restriction is ugly and non-obvious, and in the
   * future may be unnecessary.
   */
  def empty[T: Order: Ring] =
    Interval(OpenBelow(Ring[T].zero), OpenAbove(Ring[T].zero))

  /**
   * Interval containing all values above (greater-than) a.
   */
  def above[T: Order](a: T) = Interval(OpenBelow(a), UnboundAbove())

  /**
   * Interval containing all values below (less-than) a.
   */
  def below[T: Order](a: T) = Interval(UnboundBelow(), OpenAbove(a))
}

trait IntervalInstances {
  implicit def IntervalIsRig[A: Order: Ring] = new IntervalIsRig[A] {
    val o = Order[A]
    val r = Ring[A]
  }
}

// we don't support Ring[Interval[A]] due to the lack of reliable inverses
// (which is due to the dependency problem for interval arithmetic).
trait IntervalIsRig[A] extends Rig[Interval[A]] {
  implicit def o: Order[A]
  implicit def r: Ring[A]

  def one: Interval[A] = Interval.point(r.one)
  def plus(a:Interval[A], b:Interval[A]): Interval[A] = a + b
  override def pow(a:Interval[A], b:Int):Interval[A] = a.pow(b)
  override def times(a:Interval[A], b:Interval[A]): Interval[A] = a * b
  def zero: Interval[A] = Interval.point(r.zero)
}
