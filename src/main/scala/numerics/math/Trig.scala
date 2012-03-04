package numerics.math

import scala.{specialized => spec, math => mth}

trait Trig[@spec(Float,Double) A] {
  val f:Fractional[A]

  def e:A
  def pi:A

  def sin(a:A):A
  def cos(a:A):A
  def tan(a:A):A

  def asin(a:A):A
  def acos(a:A):A
  def atan(a:A):A
  def atan2(y:A, x:A):A

  def sinh(x:A):A
  def cosh(x:A):A
  def tanh(x:A):A

  def toRadians(a:A):A
  def toDegrees(a:A):A
}

object Trig {
  implicit object FloatIsTrig extends FloatIsTrig
  implicit object DoubleIsTrig extends DoubleIsTrig
  implicit object BigDecimalIsTrig extends BigDecimalIsTrig

  def apply[A](implicit t:Trig[A]) = t
}

trait FloatIsTrig extends Trig[Float] {
  val f = Fractional.FloatIsFractional

  def e:Float = mth.E.toFloat
  def pi:Float = mth.Pi.toFloat

  def exp(a:Float):Float = mth.exp(a.toDouble).toFloat

  def sin(a:Float):Float = mth.sin(a.toDouble).toFloat
  def cos(a:Float):Float = mth.cos(a.toDouble).toFloat
  def tan(a:Float):Float = mth.tan(a.toDouble).toFloat

  def asin(a:Float):Float = mth.asin(a.toDouble).toFloat
  def acos(a:Float):Float = mth.acos(a.toDouble).toFloat
  def atan(a:Float):Float = mth.atan(a.toDouble).toFloat
  def atan2(y:Float, x:Float):Float = mth.atan2(y.toDouble, x.toDouble).toFloat

  def sinh(x:Float):Float = mth.sinh(x.toDouble).toFloat
  def cosh(x:Float):Float = mth.cosh(x.toDouble).toFloat
  def tanh(x:Float):Float = mth.tanh(x.toDouble).toFloat

  def toRadians(a:Float):Float = (a * 2 * pi) / 360
  def toDegrees(a:Float):Float = (a * 360) / (2 * pi)
}

trait DoubleIsTrig extends Trig[Double] {
  val f = Fractional.DoubleIsFractional

  def e:Double = mth.E
  def pi:Double = mth.Pi

  def exp(a:Double):Double = mth.exp(a)

  def sin(a:Double):Double = mth.sin(a)
  def cos(a:Double):Double = mth.cos(a)
  def tan(a:Double):Double = mth.tan(a)

  def asin(a:Double):Double = mth.asin(a)
  def acos(a:Double):Double = mth.acos(a)
  def atan(a:Double):Double = mth.atan(a)
  def atan2(y:Double, x:Double):Double = mth.atan2(y, x)

  def sinh(x:Double):Double = mth.sinh(x)
  def cosh(x:Double):Double = mth.cosh(x)
  def tanh(x:Double):Double = mth.tanh(x)

  def toRadians(a:Double):Double = (a * 2 * pi) / 360
  def toDegrees(a:Double):Double = (a * 360) / (2 * pi)
}

// ugh. right now this is SUPER hand-wavy.
trait BigDecimalIsTrig extends Trig[BigDecimal] {
  val f = Fractional.BigDecimalIsFractional

  // TODO: ugh. use a method of generating pi to arbitrary digits instead of
  // hardcoding the string. although hardcoding the string may be faster in
  // many cases (we could easily hardcode MUCH longer versions of pi and e).
  private val piString = "3.1415926535897932384626433832795028841972"
  private val eString = "2.7182818284590452353602874713526624977572"

  val pi:BigDecimal = BigDecimal(piString)
  val e:BigDecimal = BigDecimal(eString)

  private val zero = BigDecimal(0)
  private val one = BigDecimal(1)
  private val two = BigDecimal(2)
  private val twoPi:BigDecimal = two * pi

  // TODO: ugh... BigDecimal has no useful pow()/exp() function
  def exp(a:BigDecimal):BigDecimal = BigDecimal(mth.exp(a.toDouble), a.mc)

  def toRadians(a:BigDecimal):BigDecimal = (a * twoPi) / BigDecimal(360)
  def toDegrees(a:BigDecimal):BigDecimal = (a * BigDecimal(360)) / twoPi

  // we can avoid overflow and minimize fp-error via %2pi
  // TODO: maybe use a more precise formulation of sin/cos/tan?
  private def modTwoPi(n:BigDecimal) = (n % twoPi).toDouble
  def sin(a:BigDecimal):BigDecimal = BigDecimal(mth.sin(modTwoPi(a)), a.mc)
  def cos(a:BigDecimal):BigDecimal = BigDecimal(mth.cos(modTwoPi(a)), a.mc)
  def tan(a:BigDecimal):BigDecimal = BigDecimal(mth.tan(modTwoPi(a)), a.mc)

  // 'a' will range from -1.0 to 1.0
  // TODO: maybe use a more precise formulation of asin/acos/atan?
  def asin(a:BigDecimal):BigDecimal = BigDecimal(mth.asin(a.toDouble), a.mc)
  def acos(a:BigDecimal):BigDecimal = BigDecimal(mth.acos(a.toDouble), a.mc)
  def atan(a:BigDecimal):BigDecimal = BigDecimal(mth.atan(a.toDouble), a.mc)

  def atan2(y:BigDecimal, x:BigDecimal):BigDecimal = if (x > zero) {
    atan(y / x)
  } else if (x < zero) {
    if (y >= zero) atan(y / x) + pi else atan(y / x) - pi
  } else {
    if (y > zero) pi / two else if (y < zero) -pi / two else zero
  }

  def sinh(x:BigDecimal):BigDecimal = {
    val ex = exp(x)
    (ex * ex - one) / (two * ex)
  }
  def cosh(x:BigDecimal):BigDecimal = {
    val ex = exp(x)
    (ex * ex + one) / (two * ex)
  }
  def tanh(x:BigDecimal):BigDecimal = {
    val ex = exp(x)
    val ex2 = ex * ex
    (ex2 - one) / (ex2 + one)
  }
}
