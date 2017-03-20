package spire.math

import java.math.MathContext

trait IsInQ[A] extends IsInR[A] {

  def rationalApprox(a: A, eps: Rational): Rational = toRational(a)
  def intervalRationalApprox(a: A, eps: Rational): Interval[Rational] = Interval.point(toRational(a))
  def toRational(a: A): Rational

}

object IsInQ {

  implicit object DoubleIsInQ extends IsInQ[Double] {
    def toRational(a: Double): Rational = Rational(a)
    def doubleApprox(a: Double): Double = a
    def bigDecimalApprox(a: Double, scale: Int): BigDecimal = BigDecimal(a, new MathContext(scale))
  }

  implicit object FloatIsInQ extends IsInQ[Float] {
    def doubleApprox(a: Float): Double = a.toDouble
    def bigDecimalApprox(a: Float, scale: Int): BigDecimal = BigDecimal(a, new MathContext(scale))
    def toRational(a: Float): Rational = Rational(a)
  }

  implicit object BigDecimalIsInQ extends IsInQ[BigDecimal] {
    def doubleApprox(a: BigDecimal): Double = a.toDouble
    def bigDecimalApprox(a: BigDecimal, scale: Int): BigDecimal = a
    def toRational(a: BigDecimal): Rational = Rational(a)
  }

}
