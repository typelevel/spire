package spire.math

import spire.algebra._
import spire.std._
import spire.macrosk.Ops

import scala.{specialized => spec}

trait Integral[@spec(Int,Long) A] extends EuclideanRing[A]
with ConvertableFrom[A] with ConvertableTo[A] with IsReal[A] {
  def isNonzero(x: A) = neqv(x, zero)
  def isPositive(x: A) = gt(x, zero)
  def isNegative(x: A) = lt(x, zero)
}

object Integral {
  implicit final val IntIsIntegral = new IntIsIntegral
  implicit final val LongIsIntegral = new LongIsIntegral
  implicit final val BigIntIsIntegral = new BigIntIsIntegral
  implicit final val SafeLongIsIntegral = new SafeLongIsIntegral

  @inline final def apply[A](implicit ev:Integral[A]) = ev
}

@SerialVersionUID(0L)
private[math] class IntIsIntegral extends Integral[Int] with IntIsEuclideanRing
with ConvertableFromInt with ConvertableToInt with IntIsReal with Serializable {
  override def fromInt(n: Int): Int = n
  override def toDouble(n: Int): Double = n.toDouble
}

@SerialVersionUID(0L)
private[math] class LongIsIntegral extends Integral[Long] with LongIsEuclideanRing
with ConvertableFromLong with ConvertableToLong with LongIsReal with Serializable {
  override def fromInt(n: Int): Long = n.toLong
  override def toDouble(n: Long): Double = n.toDouble
}

@SerialVersionUID(0L)
private[math] class BigIntIsIntegral extends Integral[BigInt] with BigIntIsEuclideanRing
with ConvertableFromBigInt with ConvertableToBigInt with BigIntIsReal with Serializable {
  override def fromInt(n: Int): BigInt = BigInt(n)
  override def toDouble(n: BigInt): Double = n.toDouble
}

@SerialVersionUID(0L)
private[math] class SafeLongIsIntegral extends Integral[SafeLong] with SafeLongIsEuclideanRing
with ConvertableFromSafeLong with ConvertableToSafeLong with SafeLongIsReal with Serializable {
  override def fromInt(n: Int): SafeLong = SafeLong(n)
  override def toDouble(n: SafeLong): Double = n.toDouble
}
