package spire.math

import scala.{specialized => spec}

trait Fractional[@spec(Float,Double) A] extends Field[A]
with ConvertableFrom[A] with ConvertableTo[A] with Order[A] {
  def ceil(a:A):A
  def floor(a:A):A
}

class FractionalOps[@spec(Int,Long,Float,Double) A](lhs:A)(implicit ev:Fractional[A]) {
  def ceil = ev.ceil(lhs)
  def floor = ev.floor(lhs)
}

object Fractional {
  implicit object FloatIsFractional extends FloatIsFractional
  implicit object DoubleIsFractional extends DoubleIsFractional
  implicit object BigDecimalIsFractional extends BigDecimalIsFractional
  implicit object RationalIsFractional extends RationalIsFractional
  implicit object RealIsFractional extends RealIsFractional

  def apply[A](implicit f:Fractional[A]) = f
}

trait FloatIsFractional extends Fractional[Float] with FloatIsField
with ConvertableFromFloat with ConvertableToFloat with FloatOrder {
  override def fromInt(n: Int): Float = n
  def ceil(a:Float) = scala.math.ceil(a).toFloat
  def floor(a:Float) = scala.math.floor(a).toFloat
}

trait DoubleIsFractional extends Fractional[Double] with DoubleIsField
with ConvertableFromDouble with ConvertableToDouble with DoubleOrder {
  override def fromInt(n: Int): Double = n
  def ceil(a:Double) = scala.math.ceil(a)
  def floor(a:Double) = scala.math.floor(a)
}


/**
 * A generic implementation of ceil/floor that can be mixed in.
 */
trait GenericCeilAndFloor[A] { self: Fractional[A] =>
  def ceil(a: A) = {
    val i = self.fromBigInt(self.toBigInt(a))

    if (i == a || self.lt(a, self.fromInt(0))) {
      i
    } else {
      self.plus(i, self.fromInt(1))
    }
  }

  def floor(a: A) = {
    val i = self.fromBigInt(self.toBigInt(a))

    if (i == a || self.gt(a, self.fromInt(0))) {
      i
    } else {
      self.plus(i, self.fromInt(1))
    }
  }
}

trait BigDecimalIsFractional extends Fractional[BigDecimal] with BigDecimalIsField
with ConvertableFromBigDecimal with ConvertableToBigDecimal with BigDecimalOrder {
  override def fromInt(n: Int): BigDecimal = super[ConvertableToBigDecimal].fromInt(n)
  def ceil(a:BigDecimal) = {
    val (q, r) = a /% 1
    if (r > 0) q + BigDecimal(1) else q
  }
  def floor(a:BigDecimal) = a.quot(1)
}

trait RationalIsFractional extends Fractional[Rational] with RationalIsField
with ConvertableFromRational with ConvertableToRational with RationalOrder
with GenericCeilAndFloor[Rational] {
  override def fromInt(n: Int): Rational = super[ConvertableToRational].fromInt(n)
}


trait RealIsFractional extends Fractional[Real] with RealIsField
with ConvertableFromReal with ConvertableToReal with RealOrder
with GenericCeilAndFloor[Real] {
  override def fromInt(n: Int): Real = super[ConvertableToReal].fromInt(n)
}
