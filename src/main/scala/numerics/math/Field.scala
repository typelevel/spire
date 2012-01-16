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
  implicit def complexIsField[A:Fractional] = new ComplexIsField
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

class ComplexIsField[A](implicit f:Fractional[A])
extends ComplexIsEuclideanRing[A]()(f) with Field[Complex[A]] {
  def div(a:Complex[A], b:Complex[A]) = a / b
}
