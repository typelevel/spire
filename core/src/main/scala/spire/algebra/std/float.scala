package spire.algebra
package std

import java.lang.{ Math => JavaMath }

import scala.annotation.tailrec

trait FloatIsRing extends Ring[Float] {
  override def minus(a:Float, b:Float): Float = a - b
  def negate(a:Float): Float = -a
  def one: Float = 1.0F
  def plus(a:Float, b:Float): Float = a + b
  override def pow(a:Float, b:Int): Float = Math.pow(a, b).toFloat
  override def times(a:Float, b:Float): Float = a * b
  def zero: Float = 0.0F
  
  override def fromInt(n: Int): Float = n
}

trait FloatIsEuclideanRing extends EuclideanRing[Float] with FloatIsRing {
  def quot(a:Float, b:Float) = (a - (a % b)) / b
  def mod(a:Float, b:Float) = a % b
  def gcd(a:Float, b:Float):Float = _gcd(Math.abs(a), Math.abs(b))
  @tailrec private def _gcd(a:Float, b:Float):Float = if (a < 1.0F) {
    1.0F
  } else if (b == 0.0F) {
    a
  } else if (b < 1.0F) {
    1.0F
  } else {
    _gcd(b, a % b)
  }
}

trait FloatIsField extends Field[Float] with FloatIsEuclideanRing {
  override def fromDouble(n: Double): Float = n.toFloat
  def div(a:Float, b:Float) = a / b
  def ceil(a:Float): Float = Math.floor(a).toFloat
  def floor(a:Float): Float = Math.floor(a).toFloat
  def round(a:Float): Float = spire.math.round(a)
  def isWhole(a:Float) = a % 1.0 == 0.0
}

trait FloatIsNRoot extends NRoot[Float] {
  def nroot(a: Float, k: Int): Float = Math.pow(a, 1 / k.toDouble).toFloat
  override def sqrt(a: Float): Float = Math.sqrt(a).toFloat
  def log(a: Float) = Math.log(a).toFloat
  def fpow(a: Float, b: Float) = Math.pow(a, b).toFloat
}

trait FloatIsSigned extends Signed[Float] {
  def signum(a: Float): Int = JavaMath.signum(a).toInt
  def abs(a: Float): Float = if (a < 0.0f) -a else a
}

trait FloatEq extends Eq[Float] {
  def eqv(x:Float, y:Float) = x == y
  override def neqv(x:Float, y:Float) = x != y
}

trait FloatOrder extends Order[Float] with FloatEq {
  override def gt(x: Float, y: Float) = x > y
  override def gteqv(x: Float, y: Float) = x >= y
  override def lt(x: Float, y: Float) = x < y
  override def lteqv(x: Float, y: Float) = x <= y
  override def min(x: Float, y: Float) = Math.min(x, y)
  override def max(x: Float, y: Float) = Math.max(x, y)
  def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)
}

trait FloatIsReal extends FloatOrder with FloatIsSigned {
  def toDouble(x: Float): Double = x.toDouble
}

trait FloatInstances {
  implicit object FloatAlgebra extends  FloatIsField with FloatIsNRoot
  implicit object FloatIsReal extends FloatIsReal
}
