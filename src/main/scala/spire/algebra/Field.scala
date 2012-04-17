package spire.algebra

import spire.math._

import scala.{specialized => spec}

trait Field[@spec(Int,Long,Float,Double) A] extends EuclideanRing[A] {
  def div(a:A, b:A): A
  def isWhole(a:A): Boolean
  def reciprocal(a:A): A = div(one, a)

  override def multiplicative:Group[A] = new MultiplicativeGroup[A]()(this)

  override def pow(a:A, n:Int):A =
    if (n >= 0) _pow(a, n, one)
    else reciprocal(_pow(a, -n, one))
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
  implicit def complexIsField[A:Fractional] = new ComplexIsFieldCls

  def apply[A](implicit f:Field[A]):Field[A] = f
}

trait FloatIsField extends Field[Float] with FloatIsEuclideanRing {
  def div(a:Float, b:Float) = a / b
  def isWhole(a:Float) = a % 1.0 == 0.0
}

trait DoubleIsField extends Field[Double] with DoubleIsEuclideanRing {
  def div(a:Double, b:Double) = a / b
  def isWhole(a:Double) = a % 1.0 == 0.0
}

trait BigDecimalIsField extends Field[BigDecimal] with BigDecimalIsEuclideanRing {
  def div(a:BigDecimal, b:BigDecimal) = a / b
  def isWhole(a:BigDecimal) = a % 1.0 == 0.0
}

trait RationalIsField extends Field[Rational] with RationalIsEuclideanRing {
  def div(a:Rational, b:Rational) = a / b
  def isWhole(a:Rational) = a.denominator == 1
}

trait RealIsField extends Field[Real] with RealIsEuclideanRing {
  def div(a:Real, b:Real) = a / b
  def isWhole(a:Real) = a % 1 == 0
}

trait ComplexIsField[@spec(Float,Double) A]
extends ComplexIsEuclideanRing[A] with Field[Complex[A]] {
  implicit val f:Fractional[A]
  def div(a:Complex[A], b:Complex[A]) = a / b
  def isWhole(a:Complex[A]) = a.isWhole
}

class ComplexIsFieldCls[@spec(Float,Double) A]
(implicit val f:Fractional[A]) extends ComplexIsField[A]

