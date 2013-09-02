package spire.math

import spire.syntax.ring._
import spire.syntax.order._
import spire.syntax.convertableFrom._

import scala.{specialized => spec}

case class FixedScale(denom: Int) {
  if (denom < 1)
    throw new IllegalArgumentException("illegal denominator: %s" format denom)
}

class FixedPointOverflow(n: Long) extends Exception(n.toString)

object FixedPoint {
  def apply(n: Long)(implicit scale: FixedScale): FixedPoint =
    new FixedPoint(n) * scale.denom

  def apply[@spec(Float, Double) A](a: A)(implicit scale: FixedScale, fr: Fractional[A]): FixedPoint = {
    val x = a * scale.denom
    if (x < fr.fromLong(Long.MinValue) || fr.fromLong(Long.MaxValue) < x)
      throw new FixedPointOverflow(x.toLong)
    new FixedPoint(x.toLong)
  }
}

class FixedPoint(val long: Long) extends AnyVal { lhs =>
  def unary_-(): FixedPoint =
    if (long != Long.MinValue) new FixedPoint(-long)
    else throw new FixedPointOverflow(long)

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
    rhs * q + (rhs * r) / d
  }

  def *(rhs: Long): FixedPoint = {
    val n = lhs.long * rhs
    if (lhs.long == 0 || rhs == 0 || (rhs == n / lhs.long && ((lhs.long ^ rhs ^ n) & Long.MinValue) == 0))
      new FixedPoint(n)
    else
      throw new FixedPointOverflow(n)
  }

  def /(rhs: FixedPoint)(implicit scale: FixedScale): FixedPoint = {
    val d = scale.denom
    val c = lhs.long * d

    // if lhs.lone * d is small enough, just use longs.
    if (lhs.long == 0 || d == 0 || (d == c / lhs.long && (((lhs.long ^ d ^ c) & Long.MinValue) == 0)))
      return new FixedPoint(c / rhs.long)

    // TODO: it might be nice to use something a little more
    // lightweight, but this is the least error-prone thing to
    // do right now.
    val n = SafeLong(lhs.long) * d / rhs.long
    if (n < Long.MinValue || Long.MaxValue < n)
      throw new FixedPointOverflow(n.toLong)

    new FixedPoint(n.toLong)
  }

  def /(rhs: Long): FixedPoint =
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

  def toLong()(implicit scale: FixedScale): Long =
    long / scale.denom

  def toDouble()(implicit scale: FixedScale): Double =
    long.toDouble / scale.denom

  def toBigDecimal()(implicit scale: FixedScale): BigDecimal =
    BigDecimal(long) / scale.denom

  def toRational()(implicit scale: FixedScale): Rational =
    Rational(long, scale.denom)

  def pow(k: Int)(implicit scale: FixedScale): FixedPoint = {
    if (k < 0)
      throw new IllegalArgumentException("exponent %s not allowed" format k)
    k match {
      case 0 =>
        new FixedPoint(scale.denom)
      case 1 =>
        this
      case _ =>
        val g = gcd(long, scale.denom)
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

  def toString()(implicit scale: FixedScale): String = toDouble.toString
}
