package numerics.math

import scala.{specialized => spec}

trait Integral[@spec(Int,Long) A] extends EuclideanRing[A] with Order[A]

object Integral {
  implicit object IntIsIntegral extends IntIsIntegral
  implicit object LongIsIntegral extends LongIsIntegral
  implicit object BigIntIsIntegral extends BigIntIsIntegral

  def apply[A](implicit i:Integral[A]) = i
}

trait IntIsIntegral extends Integral[Int] with IntIsEuclideanRing with IntOrder
trait LongIsIntegral extends Integral[Long] with LongIsEuclideanRing with LongOrder
trait BigIntIsIntegral extends Integral[BigInt] with BigIntIsEuclideanRing with BigIntOrder
