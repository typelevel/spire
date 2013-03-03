package spire.math

import scala.{specialized => spec, math => mth}

import spire.algebra._

import java.lang.Math
import java.math.MathContext

trait Trig[@spec(Float, Double) A] {
  def e: A
  def pi: A

  def exp(a: A): A

  def sin(a: A): A
  def cos(a: A): A
  def tan(a: A): A

  def asin(a: A): A
  def acos(a: A): A
  def atan(a: A): A
  def atan2(y: A, x: A): A

  def sinh(x: A): A
  def cosh(x: A): A
  def tanh(x: A): A

  def toRadians(a: A): A
  def toDegrees(a: A): A
}

object Trig {
  implicit object FloatIsTrig extends FloatIsTrig
  implicit object DoubleIsTrig extends DoubleIsTrig
  implicit def bigDecimalIsTrig(implicit mc: MathContext = BigDecimal.defaultMathContext) =
    new BigDecimalIsTrig(mc)
  implicit object NumberIsTrig extends NumberIsTrig
  implicit def complexIsTrig[@spec(Float, Double) A: Fractional: Trig]: Trig[Complex[A]] =
    new ComplexIsTrig[A]

  @inline final def apply[A](implicit t: Trig[A]) = t
}

trait FloatIsTrig extends Trig[Float] {
  def e: Float = Math.E.toFloat
  def pi: Float = Math.PI.toFloat

  def exp(a: Float): Float = Math.exp(a.toDouble).toFloat

  def sin(a: Float): Float = Math.sin(a.toDouble).toFloat
  def cos(a: Float): Float = Math.cos(a.toDouble).toFloat
  def tan(a: Float): Float = Math.tan(a.toDouble).toFloat

  def asin(a: Float): Float = Math.asin(a.toDouble).toFloat
  def acos(a: Float): Float = Math.acos(a.toDouble).toFloat
  def atan(a: Float): Float = Math.atan(a.toDouble).toFloat
  def atan2(y: Float, x: Float): Float = Math.atan2(y.toDouble, x.toDouble).toFloat

  def sinh(x: Float): Float = Math.sinh(x.toDouble).toFloat
  def cosh(x: Float): Float = Math.cosh(x.toDouble).toFloat
  def tanh(x: Float): Float = Math.tanh(x.toDouble).toFloat

  def toRadians(a: Float): Float = (a * 2 * pi) / 360
  def toDegrees(a: Float): Float = (a * 360) / (2 * pi)
}

trait DoubleIsTrig extends Trig[Double] {
  def e: Double = Math.E
  def pi: Double = Math.PI

  def exp(a: Double): Double = Math.exp(a)

  def sin(a: Double): Double = Math.sin(a)
  def cos(a: Double): Double = Math.cos(a)
  def tan(a: Double): Double = Math.tan(a)

  def asin(a: Double): Double = Math.asin(a)
  def acos(a: Double): Double = Math.acos(a)
  def atan(a: Double): Double = Math.atan(a)
  def atan2(y: Double, x: Double): Double = Math.atan2(y, x)

  def sinh(x: Double): Double = Math.sinh(x)
  def cosh(x: Double): Double = Math.cosh(x)
  def tanh(x: Double): Double = Math.tanh(x)

  def toRadians(a: Double): Double = (a * 2 * pi) / 360
  def toDegrees(a: Double): Double = (a * 360) / (2 * pi)
}

object BigDecimalIsTrig {
  // TODO: optimize. also consider improving exp() and just using exp(1)
  private var eCache: BigDecimal = null
  private def eFromContext(mc: MathContext): BigDecimal = {
    import spire.algebra.std.bigDecimal.BigDecimalAlgebra.sqrt

    if (eCache == null) {
    } else if (mc == eCache.mc) {
      return eCache
    } else if (mc.getPrecision < eCache.mc.getPrecision) {
      return eCache.setScale(mc.getPrecision, BigDecimal.RoundingMode.HALF_UP)
    }

    val scale = BigInt(10).pow(mc.getPrecision)

    /**
     * e = sum(k=0 -> inf)(1/k!) = 1 + 1 + 1/2 + 1/6 + 1/24 + ...
     *
     * To avoid doing lots of division, we will just represent e as a
     * rational approximation (num/denom), multiplying both by each
     * new k and adding 1 to the numerator.
     *
     * We keep track of the current value of k! as n, since once n exceeds
     * the scale we have small fractions which won't be representable in
     * the current MathContext.
     */
    var num = BigInt(2)
    var denom = BigInt(1)
    var n = BigInt(1)
    var m = 2
    while (n < scale) {
      num = num * m + 1
      denom = denom * m
      n = n * m
      m += 1
    }

    val result = BigDecimal(num, mc) / BigDecimal(denom, mc)
    eCache = result
    result
  }

  private final val c: Long = 640320L
  private final val c3_over_24: Long = (c * c * c) / 24L

  private var piCache: BigDecimal = null
  private def piFromContext(mc: MathContext): BigDecimal = {
    import spire.algebra.std.bigDecimal.BigDecimalAlgebra.sqrt

    if (piCache == null) {
    } else if (mc == piCache.mc) {
      return piCache
    } else if (mc.getPrecision < piCache.mc.getPrecision) {
      return piCache.setScale(mc.getPrecision, BigDecimal.RoundingMode.HALF_UP)
    }

    /**
     * This is an implementation of Chudnovsky's algorithm for calculating
     * pi, using binary splitting. The implementation is based on the one
     * found at http://www.craig-wood.com/nick/articles/pi-chudnovsky/.
     */
    def bs(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) = {
      if (b - a == 1) {
        val (pab: BigInt, qab: BigInt) = if (a == 0)
          (BigInt(1), BigInt(1))
        else
          ((6 * a - 5) * (2 * a - 1) * (6 * a - 1), a * a * a * c3_over_24)

        val tab = pab * (a * 545140134 + 13591409)
        if ((a & 1) == 0)
          (pab, qab, tab)
        else
          (pab, qab, -tab)
      } else {
        val m = (a + b) >> 1
        val (pam, qam, tam) = bs(a, m)
        val (pmb, qmb, tmb) = bs(m, b)
        val pab = pam * pmb
        val qab = qam * qmb
        val tab = qmb * tam + pam * tmb
        (pab, qab, tab)
      }
    }

    val n = java.lang.Math.log10(c3_over_24 / 72).toInt
    val (p, q, t) = bs(0, n)
    val sqrtc = sqrt(BigDecimal(10005, mc))
    val result = (sqrtc * BigDecimal(q * 426880, mc)) / BigDecimal(t, mc)
    piCache = result
    result
  }

  // TODO: delete or move these into the BigDecimalIsTrig class
  protected[math] final val zero = BigDecimal(0)
  protected[math] final val one = BigDecimal(1)
  protected[math] final val two = BigDecimal(2)
  protected[this] final val four = BigDecimal(4)
}

// ugh. (apart from pi, e, exp) this is very imprecise.
class BigDecimalIsTrig(mc: MathContext = BigDecimal.defaultMathContext) extends Trig[BigDecimal] {
  import BigDecimalIsTrig._

  lazy val e: BigDecimal = eFromContext(mc)
  lazy val pi: BigDecimal = piFromContext(mc)
  protected[math] lazy val twoPi = pi * 2

  def exp(k: BigDecimal): BigDecimal = spire.math.exp(k)

  protected[math] val threeSixty = BigDecimal(360)
  def toRadians(a: BigDecimal): BigDecimal = (a * twoPi) / threeSixty
  def toDegrees(a: BigDecimal): BigDecimal = (a * threeSixty) / twoPi

  @inline final def modTwoPi(n: BigDecimal) = (n % twoPi).toDouble

  // we can avoid overflow and minimize fp-error via %2pi
  // TODO: use a more precise formulation of sin/cos/tan?
  def sin(a: BigDecimal): BigDecimal = BigDecimal(Math.sin(modTwoPi(a)), a.mc)
  def cos(a: BigDecimal): BigDecimal = BigDecimal(Math.cos(modTwoPi(a)), a.mc)
  def tan(a: BigDecimal): BigDecimal = BigDecimal(Math.tan(modTwoPi(a)), a.mc)

  // 'a' will range from -1.0 to 1.0
  // TODO: use a more precise formulation of asin/acos/atan?
  def asin(a: BigDecimal): BigDecimal = BigDecimal(Math.asin(a.toDouble), a.mc)
  def acos(a: BigDecimal): BigDecimal = BigDecimal(Math.acos(a.toDouble), a.mc)
  def atan(a: BigDecimal): BigDecimal = BigDecimal(Math.atan(a.toDouble), a.mc)

  def atan2(y: BigDecimal, x: BigDecimal): BigDecimal = if (x > zero) {
    atan(y / x)
  } else if (x < zero) {
    if (y >= zero) atan(y / x) + pi else atan(y / x) - pi
  } else {
    if (y > zero) pi / two else if (y < zero) -pi / two else zero
  }

  def sinh(x: BigDecimal): BigDecimal = {
    val ex = exp(x)
    (ex * ex - one) / (two * ex)
  }
  def cosh(x: BigDecimal): BigDecimal = {
    val ex = exp(x)
    (ex * ex + one) / (two * ex)
  }
  def tanh(x: BigDecimal): BigDecimal = {
    val ex = exp(x)
    val ex2 = ex * ex
    (ex2 - one) / (ex2 + one)
  }
}

trait NumberIsTrig extends Trig[Number] {
  def e: Number = Number(Math.E)
  def pi: Number = Number(Math.PI)

  def exp(a: Number): Number = Math.exp(a.toDouble)

  def sin(a: Number): Number = Math.sin(a.toDouble)
  def cos(a: Number): Number = Math.cos(a.toDouble)
  def tan(a: Number): Number = Math.tan(a.toDouble)

  def asin(a: Number): Number = Math.asin(a.toDouble)
  def acos(a: Number): Number = Math.acos(a.toDouble)
  def atan(a: Number): Number = Math.atan(a.toDouble)
  def atan2(y: Number, x: Number): Number = Math.atan2(y.toDouble, x.toDouble)

  def sinh(x: Number): Number = Math.sinh(x.toDouble)
  def cosh(x: Number): Number = Math.cosh(x.toDouble)
  def tanh(x: Number): Number = Math.tanh(x.toDouble)

  def toRadians(a: Number): Number = (a * 2 * pi) / 360
  def toDegrees(a: Number): Number = (a * 360) / (2 * pi)
}

class ComplexIsTrig[@spec(Float, Double) A](implicit f: Fractional[A], t: Trig[A]) extends Trig[Complex[A]] {
  def e: Complex[A] = new Complex[A](t.e, f.zero)
  def pi: Complex[A] = new Complex[A](t.pi, f.zero)

  def exp(a: Complex[A]): Complex[A] = a.exp

  def sin(a: Complex[A]): Complex[A] = a.sin
  def cos(a: Complex[A]): Complex[A] = a.cos
  def tan(a: Complex[A]): Complex[A] = a.tan

  def asin(a: Complex[A]): Complex[A] = a.sin
  def acos(a: Complex[A]): Complex[A] = a.cos
  def atan(a: Complex[A]): Complex[A] = a.tan
  def atan2(y: Complex[A], x: Complex[A]): Complex[A] =
    new Complex(x.real, y.imag).atan

  def sinh(x: Complex[A]): Complex[A] = x.sinh
  def cosh(x: Complex[A]): Complex[A] = x.cosh
  def tanh(x: Complex[A]): Complex[A] = x.tanh

  def toRadians(a: Complex[A]): Complex[A] = a
  def toDegrees(a: Complex[A]): Complex[A] = a
}
