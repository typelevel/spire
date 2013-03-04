package spire.std

import spire.algebra._

import java.lang.{ Math => JavaMath }

import scala.annotation.tailrec

trait DoubleIsRing extends Ring[Double] {
  override def minus(a:Double, b:Double): Double = a - b
  def negate(a:Double): Double = -a
  def one: Double = 1.0
  def plus(a:Double, b:Double): Double = a + b
  override def pow(a:Double, b:Int): Double = Math.pow(a, b)
  override def times(a:Double, b:Double): Double = a * b
  def zero: Double = 0.0

  override def fromInt(n: Int): Double = n
}

trait DoubleIsEuclideanRing extends EuclideanRing[Double] with DoubleIsRing {
  def quot(a:Double, b:Double) = (a - (a % b)) / b
  def mod(a:Double, b:Double) = a % b
  override final def gcd(a:Double, b:Double):Double = _gcd(Math.abs(a), Math.abs(b))
  @tailrec private def _gcd(a:Double, b:Double):Double = if (a < 1.0) {
    1.0
  } else if (b == 0.0) {
    a
  } else if (b < 1.0) {
    1.0
  } else {
    _gcd(b, a % b)
  }
}

trait DoubleIsField extends Field[Double] with DoubleIsEuclideanRing {
  override def fromDouble(n: Double): Double = n
  def div(a:Double, b:Double) = a / b
  def ceil(a:Double): Double = Math.floor(a)
  def floor(a:Double): Double = Math.floor(a)
  def round(a:Double): Double = spire.math.round(a)
  def isWhole(a:Double) = a % 1.0 == 0.0
}

trait DoubleIsNRoot extends NRoot[Double] {
  def nroot(a: Double, k: Int): Double = Math.pow(a, 1 / k.toDouble)
  override def sqrt(a: Double): Double = Math.sqrt(a)
  def log(a: Double) = Math.log(a)
  def fpow(a: Double, b: Double) = Math.pow(a, b)
}

trait DoubleIsTrig extends Trig[Double] {
  def e: Double = JavaMath.E
  def pi: Double = JavaMath.PI

  def exp(a: Double): Double = JavaMath.exp(a)

  def sin(a: Double): Double = JavaMath.sin(a)
  def cos(a: Double): Double = JavaMath.cos(a)
  def tan(a: Double): Double = JavaMath.tan(a)

  def asin(a: Double): Double = JavaMath.asin(a)
  def acos(a: Double): Double = JavaMath.acos(a)
  def atan(a: Double): Double = JavaMath.atan(a)
  def atan2(y: Double, x: Double): Double = JavaMath.atan2(y, x)

  def sinh(x: Double): Double = JavaMath.sinh(x)
  def cosh(x: Double): Double = JavaMath.cosh(x)
  def tanh(x: Double): Double = JavaMath.tanh(x)

  def toRadians(a: Double): Double = (a * 2 * pi) / 360
  def toDegrees(a: Double): Double = (a * 360) / (2 * pi)
}

trait DoubleEq extends Eq[Double] {
  def eqv(x:Double, y:Double) = x == y
  override def neqv(x:Double, y:Double) = x != y
}

trait DoubleOrder extends Order[Double] with DoubleEq {
  override def gt(x: Double, y: Double) = x > y
  override def gteqv(x: Double, y: Double) = x >= y
  override def lt(x: Double, y: Double) = x < y
  override def lteqv(x: Double, y: Double) = x <= y
  override def min(x: Double, y: Double) = Math.min(x, y)
  override def max(x: Double, y: Double) = Math.max(x, y)
  def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)
}

trait DoubleIsSigned extends Signed[Double] {
  def signum(a: Double): Int = JavaMath.signum(a).toInt
  def abs(a: Double): Double = if (a < 0.0) -a else a
}

trait DoubleIsReal extends IsReal[Double] with DoubleOrder with DoubleIsSigned {
  def toDouble(x: Double): Double = x
}

trait DoubleInstances {
  implicit object DoubleAlgebra extends DoubleIsField with DoubleIsNRoot with DoubleIsTrig
  implicit object DoubleIsReal extends DoubleIsReal
}
