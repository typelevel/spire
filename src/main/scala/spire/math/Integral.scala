package spire.math

import scala.{specialized => spec}

trait Integral[@spec(Int,Long) A] extends EuclideanRing[A]
with ConvertableFrom[A] with ConvertableTo[A] with Order[A] with Signed[A]

object Integral {
  implicit object IntIsIntegral extends IntIsIntegral
  implicit object LongIsIntegral extends LongIsIntegral
  implicit object BigIntIsIntegral extends BigIntIsIntegral

  def apply[A](implicit i:Integral[A]) = i
}

trait IntIsIntegral extends Integral[Int] with IntIsEuclideanRing
with ConvertableFromInt with ConvertableToInt with IntOrder with IntIsSigned {
  override def fromInt(n: Int): Int = n
}

trait LongIsIntegral extends Integral[Long] with LongIsEuclideanRing
with ConvertableFromLong with ConvertableToLong with LongOrder with LongIsSigned {
  override def fromInt(n: Int): Long = n
}

trait BigIntIsIntegral extends Integral[BigInt] with BigIntIsEuclideanRing
with ConvertableFromBigInt with ConvertableToBigInt with BigIntOrder with BigIntIsSigned {
  override def fromInt(n: Int): BigInt = super[ConvertableToBigInt].fromInt(n)
}
