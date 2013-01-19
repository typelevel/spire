package spire.algebra

import spire.math._
import spire.macrosk.Ops
import java.lang.Math

import scala.{specialized => spec}

trait Field[@spec(Int,Long,Float,Double) A] extends EuclideanRing[A] with MultiplicativeAbGroup[A] {
  def ceil(a:A): A
  def floor(a:A): A
  def round(a:A): A
  def isWhole(a:A): Boolean
}

final class FieldOps[A](lhs:A)(implicit ev:Field[A]) {
  def isWhole() = macro Ops.unop[Boolean]
  def ceil() = macro Ops.unop[A]
  def floor() = macro Ops.unop[A]
  def round() = macro Ops.unop[A]
}

object Field extends Field0 with FieldProductImplicits {
  implicit object FloatIsField extends FloatIsField
  implicit object DoubleIsField extends DoubleIsField
  implicit object BigDecimalIsField extends BigDecimalIsField
  implicit object RationalIsField extends RationalIsField
  implicit object RealIsField extends RealIsField
  implicit def complexIsField[@spec(Float, Double) A: Fractional: Trig] =
    new ComplexIsField[A] {
      val f = Fractional[A]
      val t = Trig[A]
    }
  implicit object NumberIsField extends NumberIsField

  @inline final def apply[A](implicit f:Field[A]):Field[A] = f
}

trait Field0 {
  implicit def vectorSpaceScalar[V, @spec(Float,Double) F](implicit
      vectorSpace: VectorSpace[V, F]): Field[F] = vectorSpace.scalar
}

trait FloatIsField extends Field[Float] with FloatIsEuclideanRing {
  def div(a:Float, b:Float) = a / b
  def ceil(a:Float): Float = Math.floor(a).toFloat
  def floor(a:Float): Float = Math.floor(a).toFloat
  def round(a:Float): Float = spire.math.round(a)
  def isWhole(a:Float) = a % 1.0 == 0.0
}

trait DoubleIsField extends Field[Double] with DoubleIsEuclideanRing {
  def div(a:Double, b:Double) = a / b
  def ceil(a:Double): Double = Math.floor(a)
  def floor(a:Double): Double = Math.floor(a)
  def round(a:Double): Double = spire.math.round(a)
  def isWhole(a:Double) = a % 1.0 == 0.0
}

import java.math.MathContext
import java.math.RoundingMode.{CEILING, FLOOR, HALF_UP}
trait BigDecimalIsField extends Field[BigDecimal] with BigDecimalIsEuclideanRing {
  def div(a:BigDecimal, b:BigDecimal) = a / b
  def ceil(a:BigDecimal): BigDecimal = a.round(new MathContext(0, CEILING))
  def floor(a:BigDecimal): BigDecimal = a.round(new MathContext(0, FLOOR))
  def round(a:BigDecimal): BigDecimal = a.round(new MathContext(0, HALF_UP))
  def isWhole(a:BigDecimal) = a % 1.0 == 0.0
}

trait RationalIsField extends Field[Rational] with RationalIsEuclideanRing {
  def div(a:Rational, b:Rational) = a / b
  def ceil(a:Rational): Rational = a.ceil
  def floor(a:Rational): Rational = a.floor
  def round(a:Rational): Rational = a.round
  def isWhole(a:Rational) = a.denominator == 1
}

trait RealIsField extends Field[Real] with RealIsEuclideanRing {
  def div(a:Real, b:Real) = a / b
  def ceil(a:Real) = if (a % 1 == 0) a else a + 1 - (a % 1)
  def floor(a:Real) = a - (a % 1)
  def round(a:Real) = {
    val m = a % 1
    if (m < 0.5) a - m else a + 1 - m
  }
  def isWhole(a:Real) = a % 1 == 0
}

trait ComplexIsField[@spec(Float,Double) A]
extends ComplexIsEuclideanRing[A] with Field[Complex[A]] {
  def div(a:Complex[A], b:Complex[A]) = a / b
  def ceil(a:Complex[A]): Complex[A] = a.ceil
  def floor(a:Complex[A]): Complex[A] = a.floor
  def round(a:Complex[A]): Complex[A] = a.round
  def isWhole(a:Complex[A]) = a.isWhole
}

trait NumberIsField extends Field[Number] with NumberIsEuclideanRing {
  def div(a:Number, b:Number) = a / b
  def ceil(a:Number): Number = a.ceil
  def floor(a:Number): Number = a.floor
  def round(a:Number): Number = a.round
  def isWhole(a:Number) = a.isWhole
}
