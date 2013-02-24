package spire.math

import scala.{specialized => spec, math => mth}
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
  implicit def bigDecimalIsTrig(implicit mc: MathContext) =
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
    import spire.algebra.NRoot.BigDecimalIsNRoot.sqrt

    if (eCache == null) {
    } else if (mc == eCache.mc) {
      return eCache
    } else if (mc.getPrecision < eCache.mc.getPrecision) {
      return eCache.setScale(mc.getPrecision, BigDecimal.RoundingMode.HALF_UP)
    }

    val zero = BigDecimal(0, mc)
    val one = BigDecimal(1, mc)
    val two = BigDecimal(2, mc)

    var total: BigDecimal = BigDecimal(2, mc)
    var curr = two
    var next = one / curr
    var last = zero
    val p = mc.getPrecision + 1
    while (next != last) {
      last = next
      total += last
      curr += one
      next = (last / curr).setScale(p, BigDecimal.RoundingMode.FLOOR)
    }
    val result = total.setScale(mc.getPrecision, BigDecimal.RoundingMode.HALF_UP)
    eCache = result
    result
  }

  // TODO: use a faster method like the Chudnovsky algorithm
  private var piCache: BigDecimal = null
  private def piFromContext(mc: MathContext): BigDecimal = {
    import spire.algebra.NRoot.BigDecimalIsNRoot.sqrt

    if (piCache == null) {
    } else if (mc == piCache.mc) {
      return piCache
    } else if (mc.getPrecision < piCache.mc.getPrecision) {
      return piCache.setScale(mc.getPrecision, BigDecimal.RoundingMode.HALF_UP)
    }

    var a: BigDecimal = BigDecimal(1, mc)
    var b: BigDecimal = one / sqrt(BigDecimal(2, mc))
    var t: BigDecimal = BigDecimal(0.25, mc)
    var x: BigDecimal = BigDecimal(1, mc)
    while (a != b) {
      val tmp = a
      a = (a + b) / two
      b = sqrt(b * tmp)
      t -= x * ((tmp - a) pow 2)
      x *= two
    }
    val result = ((a + b) * (a + b)) / (t * four)
    piCache = result
    result
  }

  // TODO: delete or move these into the BigDecimalIsTrig class
  protected[math] final val zero = BigDecimal(0)
  protected[math] final val one = BigDecimal(1)
  protected[math] final val two = BigDecimal(2)
  protected[this] final val four = BigDecimal(4)
}

// ugh. right now this is SUPER hand-wavy.
// we should probably be paying a lot more attention to MathContext, etc.
class BigDecimalIsTrig(mc: MathContext) extends Trig[BigDecimal] {
  import BigDecimalIsTrig._

  //def e: BigDecimal = BigDecimalIsTrig.e
  lazy val e: BigDecimal = eFromContext(mc)
  lazy val pi: BigDecimal = piFromContext(mc)
  protected[math] lazy val twoPi = two * pi

  def exp(a: BigDecimal): BigDecimal = spire.math.exp(a)

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
