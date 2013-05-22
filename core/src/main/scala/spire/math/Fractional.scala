package spire.math

import spire.algebra._
import spire.std._
import spire.macrosk.Ops

import scala.{specialized => spec}
import java.lang.Math

trait Fractional[@spec(Float, Double) A] extends Field[A] with NRoot[A] with Integral[A]

object Fractional {
  private val ratCtx = ApproximationContext(Rational(1, 1000000000))

  implicit object FloatIsFractional extends FloatIsFractional
  implicit object DoubleIsFractional extends DoubleIsFractional
  implicit object BigDecimalIsFractional extends BigDecimalIsFractional
  implicit def RationalIsFractional(implicit ctx: ApproximationContext[Rational] = ratCtx) =
    new RationalIsFractional { val context = ctx }

  implicit object RealIsFractional extends RealIsFractional
  implicit object NumberIsFractional extends NumberIsFractional

  @inline final def apply[A](implicit ev:Fractional[A]) = ev
}

private[math] trait FloatIsFractional extends Fractional[Float] with FloatIsField
with FloatIsNRoot with ConvertableFromFloat with ConvertableToFloat
with FloatOrder with FloatIsSigned {
  override def fromInt(n: Int): Float = n
  override def fromDouble(n: Double): Float = n.toFloat
}

private[math] trait DoubleIsFractional extends Fractional[Double] with DoubleIsField
with DoubleIsNRoot with ConvertableFromDouble with ConvertableToDouble
with DoubleOrder with DoubleIsSigned {
  override def fromInt(n: Int): Double = n
  override def fromDouble(n: Double): Double = n
}


private[math] trait BigDecimalIsFractional extends Fractional[BigDecimal] with BigDecimalIsField
with BigDecimalIsNRoot with ConvertableFromBigDecimal with ConvertableToBigDecimal
with BigDecimalOrder with BigDecimalIsSigned {
  override def fromInt(n: Int): BigDecimal = BigDecimal(n)
  override def fromDouble(n: Double): BigDecimal = BigDecimal(n)
}

private[math] trait RationalIsFractional extends Fractional[Rational] with RationalIsField
with RationalIsNRoot with ConvertableFromRational with ConvertableToRational
with RationalIsReal {
  override def toDouble(n: Rational): Double = n.toDouble
  override def fromInt(n: Int): Rational = Rational(n)
  override def fromDouble(n: Double): Rational = Rational(n)
}


private[math] trait RealIsFractional extends Fractional[Real] with RealIsField
with RealIsNRoot with ConvertableFromReal with ConvertableToReal
with RealOrder with RealIsSigned {
  override def fromInt(n: Int): Real = Real(n)
  override def fromDouble(n: Double): Real = Real(n)
}

private[math] trait NumberIsFractional extends Fractional[Number] with NumberIsField
with NumberIsNRoot with ConvertableFromNumber with ConvertableToNumber
with NumberOrder with NumberIsSigned {
  override def fromInt(n: Int): Number = Number(n)
  override def fromDouble(n: Double): Number = Number(n)
}
