package numerics.math

import scala.{specialized => spec}

trait Fractional[@spec(Float,Double) A] extends Field[A] with Order[A]

object Fractional {
  implicit object FloatIsFractional extends FloatIsFractional
  implicit object DoubleIsFractional extends DoubleIsFractional
  implicit object BigDecimalIsFractional extends BigDecimalIsFractional
  implicit object RationalIsFractional extends RationalIsFractional
}

trait FloatIsFractional extends Fractional[Float] with FloatIsField with FloatOrder
trait DoubleIsFractional extends Fractional[Double] with DoubleIsField with DoubleOrder
trait BigDecimalIsFractional extends Fractional[BigDecimal] with BigDecimalIsField with BigDecimalOrder
trait RationalIsFractional extends Fractional[Rational] with RationalIsField with RationalOrder
