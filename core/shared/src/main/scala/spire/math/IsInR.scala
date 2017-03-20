package spire.math

trait Approx[P, R] {
  def approx[A](a: A, p: P)(implicit A: IsInR[A]): R
}

object Approx {

  implicit object RationalApprox extends Approx[Rational, Rational] {
    def approx[A](a: A, p: Rational)(implicit A: IsInR[A]): Rational = A.rationalApprox(a, p)
  }

  implicit object IntervalRationalApprox extends Approx[Rational, Interval[Rational]] {
    def approx[A](a: A, p: Rational)(implicit A: IsInR[A]): Interval[Rational] = A.intervalRationalApprox(a, p)
  }

  implicit object BigDecimalApprox extends Approx[Int, BigDecimal] {
    def approx[A](a: A, p: Int)(implicit A: IsInR[A]): BigDecimal = A.bigDecimalApprox(a, p)
  }

}


/** Elements that are a subset of the real numbers and can be approximated by Double, Rational or BigDecimal. */
trait IsInR[A] {

  /** If `a` can be represented exactly by a double floating-point number, return this number. Otherwise,
    * return the immediate successor or predecessor. */
  def doubleApprox(a: A): Double

  /** Returns a rational approximation of `a` that differs at most by `eps` from the correct value. */
  def rationalApprox(a: A, eps: Rational): Rational

  /** */
  def intervalRationalApprox(a: A, eps: Rational): Interval[Rational]

  /** Returns a decimal approximation with at least `scale` decimal digits, such that the approximation differs at most
    * by a single increment or decrement of the last digit.
    */
  def bigDecimalApprox(a: A, scale: Int): BigDecimal

}

object IsInR {

  implicit object rationalIsInR extends IsInR[Rational] {
    def doubleApprox(a: Rational): Double = a.toDouble
    def rationalApprox(a: Rational, eps: Rational): Rational = a
    def intervalRationalApprox(a: Rational, eps: Rational): Interval[Rational] = Interval.point(a)
    def bigDecimalApprox(a: Rational, scale: Int): BigDecimal = {
      val factor = SafeLong(10).pow(scale)
      val numeratorOverScale = (a * factor).round.toBigInt
      BigDecimal(numeratorOverScale.bigInteger, scale)
    }
  }

}
