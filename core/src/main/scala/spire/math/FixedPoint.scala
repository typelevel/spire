package spire.math

import spire.syntax.ring._
import spire.syntax.order._
import spire.syntax.convertableFrom._

import spire.algebra.{Order, Signed}

import scala.{specialized => spec}

class FixedPointOverflow(n: Long) extends Exception(n.toString)

case class FixedScale(denom: Int) {
  if (denom < 1)
    throw new IllegalArgumentException("illegal denominator: %s" format denom)

  implicit val ctxt: ApproximationContext[Rational] =
    ApproximationContext(Rational(1L, denom) * 2)
}

/**
 * FixedPoint is a value class that provides fixed point arithmetic
 * operations (using an implicit denominator) to unboxed Long values.
 * 
 * Working with FixedPoint values is similar to other fractional
 * types, except that most operations require an implicit FixedScale
 * instance (which provides the denominator).
 * 
 * For example:
 * 
 * // interpret FixedPoint(n) as n/1000
 * implicit val scale = FixedScale(1000)
 * 
 * // these three values are equivalent
 * val a = FixedPoint("12.345")            // decimal repr
 * val b = FixedPoint(Rational(2469, 200)) // fraction repr
 * val c = new FixedPoint(12345L)          // "raw" repr
 */
class FixedPoint(val long: Long) extends AnyVal { lhs =>
  def unary_-(): FixedPoint =
    if (long != Long.MinValue) new FixedPoint(-long)
    else throw new FixedPointOverflow(long)

  def == (rhs: FixedPoint): Boolean = lhs.long == rhs.long
  def != (rhs: FixedPoint): Boolean = lhs.long != rhs.long

  def abs: FixedPoint =
    if (long >= 0L) this
    else if (long != Long.MinValue) new FixedPoint(-long)
    else throw new FixedPointOverflow(long)

  def signum: Int =
    java.lang.Long.signum(long)

  def compare(rhs: FixedPoint): Int =
    if (lhs.long < rhs.long) -1 else if (lhs.long == rhs.long) 0 else 1

  def <(rhs: FixedPoint): Boolean = lhs.long < rhs.long
  def <=(rhs: FixedPoint): Boolean = lhs.long <= rhs.long
  def >(rhs: FixedPoint): Boolean = lhs.long > rhs.long
  def >=(rhs: FixedPoint): Boolean = lhs.long >= rhs.long

  def +(rhs: FixedPoint): FixedPoint = {
    val n = lhs.long + rhs.long
    if ((~(lhs.long ^ rhs.long) & (lhs.long ^ n)) < 0L)
      throw new FixedPointOverflow(n)
    new FixedPoint(n)
  }

  def +(rhs: Long)(implicit scale: FixedScale): FixedPoint = {
    val d = scale.denom
    val p = rhs * d
    if (rhs == 0 || d == 0 || (rhs == p / d && ((rhs ^ d ^ p) & Long.MinValue) == 0))
      lhs + new FixedPoint(p)

    val n = SafeLong(rhs) * d + lhs.long
    if (n < Long.MinValue || Long.MaxValue < n)
      throw new FixedPointOverflow(n.toLong)

    new FixedPoint(n.toLong)
  }

  def -(rhs: FixedPoint): FixedPoint = {
    val n = lhs.long - rhs.long
    if (((lhs.long ^ rhs.long) & (lhs.long ^ n)) < 0L)
      throw new FixedPointOverflow(n)
    new FixedPoint(n)
  }

  def -(rhs: Long)(implicit scale: FixedScale): FixedPoint = {
    val d = scale.denom
    val p = rhs * d
    if (rhs == 0 || d == 0 || (rhs == p / d && ((rhs ^ d ^ p) & Long.MinValue) == 0))
      return lhs - new FixedPoint(p)

    val n = SafeLong(lhs.long) - (SafeLong(rhs) * d)
    if (n < Long.MinValue || Long.MaxValue < n)
      throw new FixedPointOverflow(n.toLong)

    new FixedPoint(n.toLong)
  }

  def *(rhs: FixedPoint)(implicit scale: FixedScale): FixedPoint = {
    if (lhs.long < rhs.long) return rhs * lhs
    val d = scale.denom
    val q = lhs.long / d
    val r = lhs.long % d
    val qq = rhs * q
    val rr = try {
      (rhs * r) / d
    } catch {
      case _: FixedPointOverflow =>
        val n = (SafeLong(rhs.long) * r) / d
        if (n.isLong)
          new FixedPoint(n.toLong)
        else
          throw new FixedPointOverflow(n.toLong)
    }
    qq + rr
  }

  def *(rhs: Long): FixedPoint = {
    val n = lhs.long * rhs
    if (lhs.long == 0 || rhs == 0 || (rhs == n / lhs.long && ((lhs.long ^ rhs ^ n) & Long.MinValue) == 0))
      new FixedPoint(n)
    else
      throw new FixedPointOverflow(n)
  }

  def /(rhs: FixedPoint)(implicit scale: FixedScale): FixedPoint =
    try {
      (lhs * scale.denom) / rhs.long
    } catch {
      case _: FixedPointOverflow =>
        // TODO: it might be nice to use something a little more
        // lightweight, but this is the least error-prone thing to
        // do right now.
        val n = SafeLong(lhs.long) * scale.denom / rhs.long
        if (n < Long.MinValue || Long.MaxValue < n)
          throw new FixedPointOverflow(n.toLong)

        new FixedPoint(n.toLong)
    }

  def /(rhs: Long): FixedPoint =
    if (lhs.long == Long.MinValue && rhs == -1L)
      throw new FixedPointOverflow(lhs.long)
    else
      new FixedPoint(lhs.long / rhs)

  def %(rhs: FixedPoint): FixedPoint =
    new FixedPoint(lhs.long % rhs.long)

  def %(rhs: Long)(implicit scale: FixedScale): FixedPoint = {
    val d = scale.denom
    val p = rhs * d
    if (rhs == 0 || d == 0 || (d == p / rhs && (((rhs ^ d ^ p) & Long.MinValue) == 0)))
      new FixedPoint(lhs.long % p)
    else
      lhs
  }

  def /~(rhs: FixedPoint)(implicit scale: FixedScale): FixedPoint = (lhs - lhs % rhs) / rhs

  def /%(rhs: FixedPoint)(implicit scale: FixedScale): (FixedPoint, FixedPoint) = {
    val rem = lhs % rhs
    ((lhs - rem) / rhs, rem)
  }

  def isWhole(implicit scale: FixedScale): Boolean =
    long % scale.denom == 0L

  def floor(implicit scale: FixedScale): FixedPoint =
    if (long % scale.denom == 0L) this
    else if (long > 0L) FixedPoint(long / scale.denom)
    else FixedPoint(long / scale.denom - 1L)

  def ceil(implicit scale: FixedScale): FixedPoint =
    if (long % scale.denom == 0L) this
    else if (long > 0L) FixedPoint(long / scale.denom + 1L)
    else FixedPoint(long / scale.denom)

  def round(implicit scale: FixedScale): FixedPoint = {
    val d = scale.denom
    if (long % d == 0L) {
      this
    } else if (long > 0) {
      val m = (long % d)
      if (m >= (d - m)) FixedPoint(long / d + 1L) else FixedPoint(long / d)
    } else {
      val m = -(long % d)
      if (m >= (d - m)) FixedPoint(long / d - 1L) else FixedPoint(long / d)
    }
  }

  def gcd(rhs: FixedPoint): FixedPoint =
    new FixedPoint(spire.math.gcd(lhs.long, rhs.long))

  def toLong(implicit scale: FixedScale): Long =
    long / scale.denom

  def toDouble(implicit scale: FixedScale): Double =
    long.toDouble / scale.denom

  def toBigDecimal(implicit scale: FixedScale): BigDecimal =
    BigDecimal(long) / scale.denom

  def toRational(implicit scale: FixedScale): Rational =
    Rational(long, scale.denom)

  def **(k: Int)(implicit scale: FixedScale): FixedPoint = pow(k)

  def pow(k: Int)(implicit scale: FixedScale): FixedPoint = {
    if (k < 0)
      throw new IllegalArgumentException("exponent %s not allowed" format k)
    k match {
      case 0 =>
        new FixedPoint(scale.denom)
      case 1 =>
        this
      case _ =>
        val g = spire.math.gcd(long, scale.denom)
        val n = spire.math.pow((long / g).toDouble, k) * g
        val d = spire.math.pow((scale.denom / g).toDouble, k - 1)
        val x = n / d
        if (x < Long.MinValue || Long.MaxValue < x)
          throw new FixedPointOverflow(x.toLong)
        else if ((long > scale.denom || long < -scale.denom) && x.toLong == 0)
          throw new FixedPointOverflow(0L)
        new FixedPoint(x.toLong)
    }
  }

  import spire.syntax.nroot._

  def sqrt(implicit scale: FixedScale): FixedPoint = {
    import scale.ctxt
    FixedPoint(toRational.sqrt)
  }

  def nroot(k: Int)(implicit scale: FixedScale): FixedPoint = {
    import scale.ctxt
    FixedPoint(toRational.nroot(k))
  }

  def fpow(y: FixedPoint)(implicit scale: FixedScale): FixedPoint = {
    import scale.ctxt
    FixedPoint(toRational.fpow(y.toRational))
  }

  override def toString: String = long.toString + "/?"

  def toString(implicit scale: FixedScale): String = toDouble.toString
}

object FixedPoint extends FixedPointInstances {

  val zero: FixedPoint = new FixedPoint(0L)

  val MaxValue: FixedPoint = new FixedPoint(Long.MaxValue)
  val MinValue: FixedPoint = new FixedPoint(Long.MinValue)

  def one(implicit scale: FixedScale): FixedPoint = new FixedPoint(scale.denom)

  def apply(n: Long)(implicit scale: FixedScale): FixedPoint =
    new FixedPoint(n) * scale.denom

  def apply(n: Rational)(implicit scale: FixedScale): FixedPoint = {
    val x = (n * scale.denom).round
    if (x < Long.MinValue || x > Long.MaxValue)
      throw new FixedPointOverflow(x.toLong)
    new FixedPoint(x.toLong)
  }

  def apply(s: String)(implicit scale: FixedScale): FixedPoint =
    apply(Rational(s))

  def apply[@spec(Float, Double) A](a: A)(implicit scale: FixedScale, fr: Fractional[A]): FixedPoint = {
    val x = a * scale.denom
    if (x < fr.fromLong(Long.MinValue) || fr.fromLong(Long.MaxValue) < x)
      throw new FixedPointOverflow(x.toLong)
    new FixedPoint(x.toLong)
  }
}

trait FixedPointInstances {

  implicit def algebra(implicit scale: FixedScale) =
    new Fractional[FixedPoint] with Order[FixedPoint] with Signed[FixedPoint] {
      implicit val ctxt: ApproximationContext[Rational] =
        ApproximationContext(Rational(1L, scale.denom) * 2)

      def abs(x: FixedPoint): FixedPoint = x.abs
      def signum(x: FixedPoint): Int = x.signum

      override def eqv(x: FixedPoint, y: FixedPoint): Boolean = x == y
      def compare(x: FixedPoint, y: FixedPoint): Int = x compare y

      def zero: FixedPoint = FixedPoint.zero
      def one: FixedPoint = FixedPoint.one
      def negate(x: FixedPoint): FixedPoint = -x
      def plus(x: FixedPoint, y: FixedPoint): FixedPoint = x + y
      override def minus(x: FixedPoint, y: FixedPoint): FixedPoint = x - y
      def times(x: FixedPoint, y: FixedPoint): FixedPoint = x * y

      def gcd(x: FixedPoint, y: FixedPoint): FixedPoint = x gcd y
      def quot(x: FixedPoint, y: FixedPoint): FixedPoint = x /~ y
      def mod(x: FixedPoint, y: FixedPoint): FixedPoint = x % y

      override def reciprocal(x: FixedPoint): FixedPoint = one / x
      def div(x: FixedPoint, y: FixedPoint): FixedPoint = x / y

      override def sqrt(x: FixedPoint): FixedPoint = x.sqrt
      def nroot(x: FixedPoint, k: Int): FixedPoint = x.nroot(k)
      def fpow(x: FixedPoint, y: FixedPoint): FixedPoint = x.fpow(y)

      def ceil(x: FixedPoint): FixedPoint = x.ceil
      def floor(x: FixedPoint): FixedPoint = x.floor
      def isWhole(x: FixedPoint): Boolean = x.isWhole
      def round(x: FixedPoint): FixedPoint = x.round

      def toByte(x: FixedPoint): Byte = x.toRational.toByte
      def toShort(x: FixedPoint): Short = x.toRational.toShort
      def toInt(x: FixedPoint): Int = x.toRational.toInt
      def toLong(x: FixedPoint): Long = x.toRational.toLong
      def toFloat(x: FixedPoint): Float = x.toRational.toFloat
      def toDouble(x: FixedPoint): Double = x.toRational.toDouble
      def toBigInt(x: FixedPoint): BigInt = x.toRational.toBigInt
      def toBigDecimal(x: FixedPoint): BigDecimal = x.toRational.toBigDecimal
      def toRational(x: FixedPoint): Rational = x.toRational
      def toAlgebraic(x: FixedPoint): Algebraic = Algebraic(x.toRational)
      def toReal(x: FixedPoint): Real = Real(x.toRational)
      def toNumber(x: FixedPoint): Number = Number(x.toRational)
      def toString(x: FixedPoint): String = x.toString

      def toType[B](x: FixedPoint)(implicit ev: ConvertableTo[B]): B =
        ev.fromRational(x.toRational)

      def fromByte(n: Byte): FixedPoint = FixedPoint(n)
      def fromShort(n: Short): FixedPoint = FixedPoint(n)
      def fromFloat(n: Float): FixedPoint = FixedPoint(n)
      def fromLong(n: Long): FixedPoint = FixedPoint(n)
      def fromBigInt(n: BigInt): FixedPoint = FixedPoint(BigDecimal(n))
      def fromBigDecimal(n: BigDecimal): FixedPoint = FixedPoint(n)
      def fromRational(n: Rational): FixedPoint = FixedPoint(n)
      def fromAlgebraic(n: Algebraic): FixedPoint = FixedPoint(n.toRational)
      def fromReal(n: Real): FixedPoint = FixedPoint(n.toRational)

      def fromType[B](b: B)(implicit ev: ConvertableFrom[B]): FixedPoint =
        FixedPoint(ev.toRational(b))
    }

  import NumberTag._
  implicit final val FixedPointTag = new CustomTag[FixedPoint](
    Approximate, Some(FixedPoint.zero),
    Some(FixedPoint.MinValue), Some(FixedPoint.MaxValue), true, true)
}
