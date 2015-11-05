package spire
package math

import spire.algebra.{Field, NRoot}
import spire.std._

import java.lang.Math

trait Fractional[@sp(Float, Double) A] extends Any with Field[A] with NRoot[A] with Integral[A]

object Fractional {
  implicit final val FloatIsFractional = new FloatIsFractional
  implicit final val DoubleIsFractional = new DoubleIsFractional
  implicit final val BigDecimalIsFractional = new BigDecimalIsFractional
  implicit final val AlgebraicIsFractional = new AlgebraicIsFractional
  implicit final val NumberIsFractional = new NumberIsFractional
  implicit final val RationalIsFractional = new RationalIsFractional

  @inline final def apply[A](implicit ev:Fractional[A]): Fractional[A] = ev
}

@SerialVersionUID(0L)
private[math] class FloatIsFractional extends Fractional[Float] with FloatIsField
with FloatIsNRoot with ConvertableFromFloat with ConvertableToFloat
with FloatIsReal with Serializable {
  override def fromInt(n: Int): Float = n
  override def fromDouble(n: Double): Float = n.toFloat
  override def toDouble(n: Float): Double = n.toDouble
  override def toRational(n: Float): Rational = super[FloatIsReal].toRational(n)
  override def toAlgebraic(n: Float): Algebraic = super[FloatIsReal].toAlgebraic(n)
  override def toReal(n: Float): Real = super[FloatIsReal].toReal(n)
}

@SerialVersionUID(0L)
private[math] class DoubleIsFractional extends Fractional[Double] with DoubleIsField
with DoubleIsNRoot with ConvertableFromDouble with ConvertableToDouble
with DoubleIsReal with Serializable {
  override def fromInt(n: Int): Double = n
  override def fromDouble(n: Double): Double = n
  override def toDouble(n: Double): Double = n.toDouble
  override def toRational(n: Double): Rational = super[DoubleIsReal].toRational(n)
  override def toAlgebraic(n: Double): Algebraic = super[DoubleIsReal].toAlgebraic(n)
  override def toReal(n: Double): Real = super[DoubleIsReal].toReal(n)
}

@SerialVersionUID(0L)
private[math] class BigDecimalIsFractional extends Fractional[BigDecimal] with BigDecimalIsField
with BigDecimalIsNRoot with ConvertableFromBigDecimal with ConvertableToBigDecimal
with BigDecimalIsReal with Serializable {
  override def fromInt(n: Int): BigDecimal = BigDecimal(n)
  override def fromDouble(n: Double): BigDecimal = BigDecimal(n)
  override def toDouble(n: BigDecimal): Double = n.toDouble
  override def toRational(n: BigDecimal): Rational = super[BigDecimalIsReal].toRational(n)
  override def toAlgebraic(n: BigDecimal): Algebraic = super[BigDecimalIsReal].toAlgebraic(n)
  override def toReal(n: BigDecimal): Real = super[BigDecimalIsReal].toReal(n)
}

@SerialVersionUID(1L)
private[math] class RationalIsFractional extends Fractional[Rational]
    with RationalIsField
    with RationalApproximateNRoot
    with ConvertableFromRational with ConvertableToRational
    with RationalIsReal with Serializable {

  override def fromInt(n: Int): Rational = Rational(n)
  override def fromDouble(n: Double): Rational = Rational(n)
  override def toDouble(n: Rational): Double = n.toDouble
  override def toRational(n: Rational): Rational = n
  override def toAlgebraic(n: Rational): Algebraic = super[RationalIsReal].toAlgebraic(n)
  override def toReal(n: Rational): Real = super[RationalIsReal].toReal(n)
}

@SerialVersionUID(1L)
private[math] class AlgebraicIsFractional extends Fractional[Algebraic] with AlgebraicIsFieldWithNRoot
with ConvertableFromAlgebraic with ConvertableToAlgebraic
with AlgebraicIsReal with Serializable {
  override def fromInt(n: Int): Algebraic = Algebraic(n)
  override def fromDouble(n: Double): Algebraic = Algebraic(n)
  override def toDouble(n: Algebraic): Double = n.toDouble
  override def toAlgebraic(n: Algebraic): Algebraic = n
  override def toReal(n: Algebraic): Real = super[AlgebraicIsReal].toReal(n)
}

@SerialVersionUID(0L)
private[math] class NumberIsFractional extends Fractional[Number] with NumberIsField
with NumberIsNRoot with ConvertableFromNumber with ConvertableToNumber
with NumberIsReal with Serializable {
  override def fromInt(n: Int): Number = Number(n)
  override def fromDouble(n: Double): Number = Number(n)
  override def toDouble(n: Number): Double = n.toDouble
  override def toRational(n: Number): Rational = super[NumberIsReal].toRational(n)
  override def toAlgebraic(n: Number): Algebraic = super[NumberIsReal].toAlgebraic(n)
  override def toReal(n: Number): Real = super[NumberIsReal].toReal(n)
}
