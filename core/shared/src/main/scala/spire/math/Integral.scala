package spire
package math

import spire.algebra.{EuclideanRing, IsReal}
import spire.std._

trait Integral[@sp(Int,Long) A] extends Any with EuclideanRing[A] with ConvertableFrom[A] with ConvertableTo[A] with IsReal[A]

object Integral {
  implicit final val IntIsIntegral = new IntIsIntegral
  implicit final val LongIsIntegral = new LongIsIntegral
  implicit final val BigIntIsIntegral = new BigIntIsIntegral
  implicit final val SafeLongIsIntegral = new SafeLongIsIntegral

  @inline final def apply[A](implicit ev: Integral[A]): Integral[A] = ev
}

class IntegralOps[A](lhs: A)(implicit ev: Integral[A]) {
  def factor: prime.Factors = prime.factor(toSafeLong)
  def isPrime: Boolean = prime.isPrime(toSafeLong)
  def toSafeLong: SafeLong = SafeLong(ev.toBigInt(lhs))

  def coerce(a: A): Long = {
    val n = ev.toBigInt(a)
    if (Long.MinValue <= n && n <= Long.MaxValue) ev.toLong(a)
    else throw new IllegalArgumentException(s"$lhs too large")
  }

  def ! : BigInt = spire.math.fact(coerce(lhs))

  def choose(rhs: A): BigInt = spire.math.choose(coerce(lhs), coerce(rhs))
}

@SerialVersionUID(0L)
private[math] class IntIsIntegral extends Integral[Int] with IntIsEuclideanRing
with ConvertableFromInt with ConvertableToInt with IntIsReal with Serializable {
  def additiveAbGroup = this
  override def fromInt(n: Int): Int = n
  override def toDouble(n: Int): Double = n.toDouble
  override def toRational(n: Int): Rational = super[IntIsReal].toRational(n)
  override def toAlgebraic(n: Int): Algebraic = super[IntIsReal].toAlgebraic(n)
  override def toReal(n: Int): Real = super[IntIsReal].toReal(n)
  override def toBigInt(n: Int): BigInt = BigInt(n)
}

@SerialVersionUID(0L)
private[math] class LongIsIntegral extends Integral[Long] with LongIsEuclideanRing
with ConvertableFromLong with ConvertableToLong with LongIsReal with Serializable {
  def additiveAbGroup = this
  override def fromInt(n: Int): Long = n.toLong
  override def toDouble(n: Long): Double = n.toDouble
  override def toRational(n: Long): Rational = super[LongIsReal].toRational(n)
  override def toAlgebraic(n: Long): Algebraic = super[LongIsReal].toAlgebraic(n)
  override def toReal(n: Long): Real = super[LongIsReal].toReal(n)
  override def toBigInt(n: Long): BigInt = BigInt(n)
}

@SerialVersionUID(0L)
private[math] class BigIntIsIntegral extends Integral[BigInt] with BigIntIsEuclideanRing
with ConvertableFromBigInt with ConvertableToBigInt with BigIntIsReal with Serializable {
  def additiveAbGroup = this
  override def fromInt(n: Int): BigInt = BigInt(n)
  override def toDouble(n: BigInt): Double = n.toDouble
  override def toRational(n: BigInt): Rational = super[BigIntIsReal].toRational(n)
  override def toAlgebraic(n: BigInt): Algebraic = super[BigIntIsReal].toAlgebraic(n)
  override def toReal(n: BigInt): Real = super[BigIntIsReal].toReal(n)
  override def toBigInt(n: BigInt): BigInt = super[BigIntIsReal].toBigInt(n)
}

@SerialVersionUID(0L)
private[math] class SafeLongIsIntegral extends Integral[SafeLong] with SafeLongIsEuclideanRing
with ConvertableFromSafeLong with ConvertableToSafeLong with SafeLongIsReal with Serializable {
  def additiveAbGroup = this
  override def fromInt(n: Int): SafeLong = SafeLong(n)
  override def toDouble(n: SafeLong): Double = n.toDouble
  override def toRational(n: SafeLong): Rational = super[SafeLongIsReal].toRational(n)
  override def toAlgebraic(n: SafeLong): Algebraic = super[SafeLongIsReal].toAlgebraic(n)
  override def toReal(n: SafeLong): Real = super[SafeLongIsReal].toReal(n)
  override def toBigInt(n: SafeLong): BigInt = super[SafeLongIsReal].toBigInt(n)
}
