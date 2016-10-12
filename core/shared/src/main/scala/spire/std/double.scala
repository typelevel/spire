package spire
package std

import spire.algebra.{Field, IsRational, NRoot, Order, Signed, Trig}
import spire.math.Rational

import java.lang.Math
import java.lang.Long.{ numberOfTrailingZeros, numberOfLeadingZeros }
import java.lang.Double.{ longBitsToDouble, doubleToLongBits }


trait DoubleIsField extends Field[Double] {
  override def minus(a:Double, b:Double): Double = a - b
  def negate(a:Double): Double = -a
  def one: Double = 1.0
  def plus(a:Double, b:Double): Double = a + b
  override def pow(a:Double, b:Int): Double = Math.pow(a, b)
  override def times(a:Double, b:Double): Double = a * b
  def zero: Double = 0.0

  override def fromInt(n: Int): Double = n

  def quot(a:Double, b:Double): Double = (a - (a % b)) / b
  def mod(a:Double, b:Double): Double = a % b

  final def gcd(a:Double, b:Double):Double = {
    def value(bits: Long): Long = bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L

    def exp(bits: Long): Int = ((bits >> 52) & 0x7FF).toInt

    // Computes the GCD of 2 fp values. Here, we are guaranteed that exp0 < exp1.
    def gcd0(val0: Long, exp0: Int, val1: Long, exp1: Int): Double = {
      val tz0 = numberOfTrailingZeros(val0)
      val tz1 = numberOfTrailingZeros(val1)
      val tzShared = spire.math.min(tz0, tz1 + exp1 - exp0)
      // We trim of the power of 2s, then add back the shared portion.
      val n = spire.math.gcd(val0 >>> tz0, val1 >>> tz1) << tzShared
      // Number of bits to move the leading 1 to bit position 23.
      val shift = numberOfLeadingZeros(n) - 11 // Number of bits to move 1 to bit 52
      val exp = (exp0 - shift).toLong
      // If exp is 0, then the value is actually just the mantissa * 2^−126,
      // so we need to adjust the *shift* accordingly.
      val shift0 = if (exp == 0) shift - 1 else shift
      val mantissa = (n << shift0) & 0x000FFFFFFFFFFFFFL
      // If exp < 0, then we have underflowed; not much we can do but return 0.
      if (exp < 0) 0.0
      else longBitsToDouble((exp << 52) | mantissa)
    }

    if (a == 0D) b
    else if (b == 0D) a
    else {
      val aBits = doubleToLongBits(a)
      val aVal = value(aBits)
      val aExp = exp(aBits)

      val bBits = doubleToLongBits(b)
      val bVal = value(bBits)
      val bExp = exp(bBits)

      if (aExp < bExp) gcd0(aVal, aExp, bVal, bExp)
      else gcd0(bVal, bExp, aVal, aExp)
    }
  }

  override def fromDouble(n: Double): Double = n
  def div(a:Double, b:Double): Double = a / b
}

trait DoubleIsNRoot extends NRoot[Double] {
  def nroot(a: Double, k: Int): Double = Math.pow(a, 1 / k.toDouble)
  override def sqrt(a: Double): Double = Math.sqrt(a)
  def fpow(a: Double, b: Double): Double = Math.pow(a, b)
}

trait DoubleIsTrig extends Trig[Double] {
  def e: Double = Math.E
  def pi: Double = Math.PI

  def exp(a: Double): Double = Math.exp(a)
  def expm1(a: Double): Double = Math.expm1(a)
  def log(a: Double): Double = Math.log(a)
  def log1p(a: Double): Double = Math.log1p(a)

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

trait DoubleOrder extends Order[Double] {
  override def eqv(x:Double, y:Double): Boolean = x == y
  override def neqv(x:Double, y:Double): Boolean = x != y
  override def gt(x: Double, y: Double): Boolean = x > y
  override def gteqv(x: Double, y: Double): Boolean = x >= y
  override def lt(x: Double, y: Double): Boolean = x < y
  override def lteqv(x: Double, y: Double): Boolean = x <= y
  override def min(x: Double, y: Double): Double = Math.min(x, y)
  override def max(x: Double, y: Double): Double = Math.max(x, y)
  def compare(x: Double, y: Double): Int = java.lang.Double.compare(x, y)
}

trait DoubleIsSigned extends Signed[Double] {
  override def signum(a: Double): Int = Math.signum(a).toInt
  override def abs(a: Double): Double = if (a < 0.0) -a else a
}

trait DoubleIsReal extends IsRational[Double] with DoubleOrder with DoubleIsSigned {
  def toDouble(x: Double): Double = x
  def ceil(a:Double): Double = Math.ceil(a)
  def floor(a:Double): Double = Math.floor(a)
  def round(a:Double): Double = spire.math.round(a)
  def isWhole(a:Double): Boolean = a % 1.0 == 0.0
  def toRational(a:Double): Rational = Rational(a)
}

@SerialVersionUID(0L)
class DoubleAlgebra extends DoubleIsField with DoubleIsNRoot with DoubleIsTrig with DoubleIsReal with Serializable

trait DoubleInstances {
  implicit final val DoubleAlgebra = new DoubleAlgebra
  import Double._
  import spire.math.NumberTag._
  implicit final val DoubleTag = new BuiltinFloatTag(0D, MinValue, MaxValue, NaN, PositiveInfinity, NegativeInfinity) {
    def isInfinite(a: Double): Boolean = java.lang.Double.isInfinite(a)
    def isNaN(a: Double): Boolean =  java.lang.Double.isNaN(a)
  }
}
