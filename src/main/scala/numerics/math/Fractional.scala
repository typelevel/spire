package numerics.math

import scala.{specialized => spec}

trait Fractional[@spec(Float,Double) A] extends Field[A] with Order[A] {
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
}

trait FloatIsFractional extends Fractional[Float] with FloatIsField with FloatOrder {
  def ceil(a:Float) = scala.math.ceil(a).toFloat
  def floor(a:Float) = scala.math.floor(a).toFloat
}

trait DoubleIsFractional extends Fractional[Double] with DoubleIsField with DoubleOrder {
  def ceil(a:Double) = scala.math.ceil(a)
  def floor(a:Double) = scala.math.floor(a)
}

trait BigDecimalIsFractional extends Fractional[BigDecimal]
with BigDecimalIsField with BigDecimalOrder {
  def ceil(a:BigDecimal) = {
    val (q, r) = a /% 1
    if (r > 0) q + BigDecimal(1) else q
  }
  def floor(a:BigDecimal) = a.quot(1)
}

trait RationalIsFractional extends Fractional[Rational]
with RationalIsField with RationalOrder {
  def ceil(a:Rational) = {
    val (q, r) = a.toBigDecimal /% 1
    Rational((if (r > 0) q + BigDecimal(1) else q).toBigInt, 1)
  }
  def floor(a:Rational) = Rational(a.toBigInt, 1)
}
