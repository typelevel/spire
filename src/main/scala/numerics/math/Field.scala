package numerics.math

import scala.{specialized => spec}

trait Field[@spec(Int,Long,Float,Double) A] extends EuclideanRing[A] {
  def div(a:A, b:A):A
  def isWhole(a:A): Boolean = eq(mod(a, one), zero)
}

final class FieldOps[@spec(Int,Long,Float,Double) A](lhs:A)(implicit ev:Field[A]) {
  def /(rhs:A) = ev.div(lhs, rhs)
  def isWhole = ev.isWhole(lhs)
}

object Field {
  implicit object FloatIsField extends FloatIsField
  implicit object DoubleIsField extends DoubleIsField
  implicit object BigDecimalIsField extends BigDecimalIsField
  implicit object RationalIsField extends RationalIsField
  implicit object RealIsField extends RealIsField
  implicit def complexIsField[A:Fractional:Exponential] = new ComplexIsFieldCls

  def apply[A](implicit f:Field[A]):Field[A] = f
}

trait FloatIsField extends Field[Float] with FloatIsEuclideanRing {
  def div(a:Float, b:Float) = a / b
}

trait DoubleIsField extends Field[Double] with DoubleIsEuclideanRing {
  def div(a:Double, b:Double) = a / b
}

trait BigDecimalIsField extends Field[BigDecimal] with BigDecimalIsEuclideanRing {
  def div(a:BigDecimal, b:BigDecimal) = a / b
}

trait RationalIsField extends Field[Rational] with RationalIsEuclideanRing {
  def div(a:Rational, b:Rational) = a / b
}

trait RealIsField extends Field[Real] with RealIsEuclideanRing {
  def div(a:Real, b:Real) = a / b
}

trait ComplexIsField[@spec(Float,Double) A]
extends ComplexIsEuclideanRing[A] with Field[Complex[A]] {
  implicit val f:Fractional[A]
  implicit val e:Exponential[A]
  def div(a:Complex[A], b:Complex[A]) = a / b
}

class ComplexIsFieldCls[@spec(Float,Double) A]
(implicit val f:Fractional[A], val e:Exponential[A]) extends ComplexIsField[A]

