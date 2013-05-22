package spire.math

import spire.algebra._
import spire.std._
import spire.macrosk.Ops

import scala.{specialized => spec}

trait Integral[@spec(Int,Long) A] extends EuclideanRing[A]
with ConvertableFrom[A] with ConvertableTo[A] with Order[A] with Signed[A] {
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
with ConvertableFromInt with ConvertableToInt with IntOrder with IntIsSigned {
  override def fromInt(n: Int): Int = n
}

private[math] trait LongIsIntegral extends Integral[Long] with LongIsEuclideanRing
with ConvertableFromLong with ConvertableToLong with LongOrder with LongIsSigned {
  override def fromInt(n: Int): Long = n.toLong
}

private[math] trait BigIntIsIntegral extends Integral[BigInt] with BigIntIsEuclideanRing
with ConvertableFromBigInt with ConvertableToBigInt with BigIntOrder with BigIntIsSigned {
  override def fromInt(n: Int): BigInt = BigInt(n)
}

private[math] trait SafeLongIsIntegral extends Integral[SafeLong] with SafeLongIsEuclideanRing
with ConvertableFromSafeLong with ConvertableToSafeLong with SafeLongOrder with SafeLongIsSigned {
  override def fromInt(n: Int): SafeLong = SafeLong(n)
}
