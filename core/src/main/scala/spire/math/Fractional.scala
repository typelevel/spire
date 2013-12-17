package spire.math

import spire.algebra._
import spire.std._
import spire.macrosk.Ops

import scala.{specialized => spec}
import java.lang.Math

trait Fractional[@spec(Float, Double) A] extends Field[A] with NRoot[A] with Integral[A]

object Fractional {
  private val ratCtx = ApproximationContext(Rational(1, 1000000000))

  implicit final val FloatIsFractional = new FloatIsFractional
  implicit final val DoubleIsFractional = new DoubleIsFractional
  implicit final val BigDecimalIsFractional = new BigDecimalIsFractional
  implicit final val AlgebraicIsFractional = new AlgebraicIsFractional
  implicit final val NumberIsFractional = new NumberIsFractional

  implicit def RationalIsFractional(implicit ctx: ApproximationContext[Rational] = ratCtx) =
    new RationalIsFractional

  @inline final def apply[A](implicit ev:Fractional[A]) = ev
}

@SerialVersionUID(0L)
private[math] class FloatIsFractional extends Fractional[Float] with FloatIsField
with FloatIsNRoot with ConvertableFromFloat with ConvertableToFloat
with FloatIsReal with Serializable {
  override def fromInt(n: Int): Float = n
  override def fromDouble(n: Double): Float = n.toFloat
  override def toDouble(n: Float): Double = n.toDouble
}

@SerialVersionUID(0L)
private[math] class DoubleIsFractional extends Fractional[Double] with DoubleIsField
with DoubleIsNRoot with ConvertableFromDouble with ConvertableToDouble
with DoubleIsReal with Serializable {
  override def fromInt(n: Int): Double = n
  override def fromDouble(n: Double): Double = n
  override def toDouble(n: Double): Double = n.toDouble
}

@SerialVersionUID(0L)
private[math] class BigDecimalIsFractional extends Fractional[BigDecimal] with BigDecimalIsField
with BigDecimalIsNRoot with ConvertableFromBigDecimal with ConvertableToBigDecimal
with BigDecimalIsReal with Serializable {
  override def fromInt(n: Int): BigDecimal = BigDecimal(n)
  override def fromDouble(n: Double): BigDecimal = BigDecimal(n)
  override def toDouble(n: BigDecimal): Double = n.toDouble
}

@SerialVersionUID(0L)
private[math] class RationalIsFractional(implicit val context: ApproximationContext[Rational])
extends Fractional[Rational] with RationalIsField with RationalIsNRoot
with ConvertableFromRational with ConvertableToRational
with RationalIsReal with Serializable {
  override def fromInt(n: Int): Rational = Rational(n)
  override def fromDouble(n: Double): Rational = Rational(n)
  override def toDouble(n: Rational): Double = n.toDouble
}

@SerialVersionUID(0L)
private[math] class AlgebraicIsFractional extends Fractional[Algebraic] with AlgebraicIsField
with AlgebraicIsNRoot with ConvertableFromAlgebraic with ConvertableToAlgebraic
with AlgebraicIsReal with Serializable {
  override def fromInt(n: Int): Algebraic = Algebraic(n)
  override def fromDouble(n: Double): Algebraic = Algebraic(n)
  override def toDouble(n: Algebraic): Double = n.toDouble
}

@SerialVersionUID(0L)
private[math] class NumberIsFractional extends Fractional[Number] with NumberIsField
with NumberIsNRoot with ConvertableFromNumber with ConvertableToNumber
with NumberIsReal with Serializable {
  override def fromInt(n: Int): Number = Number(n)
  override def fromDouble(n: Double): Number = Number(n)
  override def toDouble(n: Number): Double = n.toDouble
}
