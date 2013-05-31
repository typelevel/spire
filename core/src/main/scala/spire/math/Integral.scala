package spire.math

import spire.algebra._
import spire.std._
import spire.macrosk.Ops

import scala.{specialized => spec}

trait Integral[@spec(Int,Long) A] extends EuclideanRing[A]
with ConvertableFrom[A] with ConvertableTo[A] with IsReal[A] {
  def isZero(x: A) = eqv(x, zero)
  def isNonzero(x: A) = neqv(x, zero)
  def isPositive(x: A) = gt(x, zero)
  def isNegative(x: A) = lt(x, zero)
}

object Integral {
  implicit object IntIsIntegral extends IntIsIntegral
  implicit object LongIsIntegral extends LongIsIntegral
  implicit object BigIntIsIntegral extends BigIntIsIntegral
  implicit object SafeLongIsIntegral extends SafeLongIsIntegral

  @inline final def apply[A](implicit ev:Integral[A]) = ev
}

private[math] trait IntIsIntegral extends Integral[Int] with IntIsEuclideanRing
with ConvertableFromInt with ConvertableToInt with IntIsReal {
  override def fromInt(n: Int): Int = n
  override def toDouble(n: Int): Double = n.toDouble
}

private[math] trait LongIsIntegral extends Integral[Long] with LongIsEuclideanRing
with ConvertableFromLong with ConvertableToLong with LongIsReal {
  override def fromInt(n: Int): Long = n.toLong
  override def toDouble(n: Long): Double = n.toDouble
}

private[math] trait BigIntIsIntegral extends Integral[BigInt] with BigIntIsEuclideanRing
with ConvertableFromBigInt with ConvertableToBigInt with BigIntIsReal {
  override def fromInt(n: Int): BigInt = BigInt(n)
  override def toDouble(n: BigInt): Double = n.toDouble
}

private[math] trait SafeLongIsIntegral extends Integral[SafeLong] with SafeLongIsEuclideanRing
with ConvertableFromSafeLong with ConvertableToSafeLong with SafeLongIsReal {
  override def fromInt(n: Int): SafeLong = SafeLong(n)
  override def toDouble(n: SafeLong): Double = n.toDouble
}
