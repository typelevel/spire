package spire.algebra
package std

import scala.annotation.tailrec

import BigDecimal.RoundingMode.{CEILING, FLOOR, HALF_UP}

trait BigDecimalIsRing extends Ring[BigDecimal] {
  override def minus(a:BigDecimal, b:BigDecimal): BigDecimal = a - b
  def negate(a:BigDecimal): BigDecimal = -a
  val one: BigDecimal = BigDecimal(1.0)
  def plus(a:BigDecimal, b:BigDecimal): BigDecimal = a + b
  override def pow(a:BigDecimal, b:Int): BigDecimal = a.pow(b)
  override def times(a:BigDecimal, b:BigDecimal): BigDecimal = a * b
  val zero: BigDecimal = BigDecimal(0.0)

  override def fromInt(n: Int): BigDecimal = BigDecimal(n)
}

trait BigDecimalIsEuclideanRing extends EuclideanRing[BigDecimal] with BigDecimalIsRing {
  def quot(a:BigDecimal, b:BigDecimal) = a.quot(b)
  def mod(a:BigDecimal, b:BigDecimal) = a % b
  override def quotmod(a:BigDecimal, b:BigDecimal) = a /% b
  def gcd(a:BigDecimal, b:BigDecimal):BigDecimal = _gcd(a.abs, b.abs)
  @tailrec private def _gcd(a:BigDecimal, b:BigDecimal):BigDecimal = {
    if (a < one) {
      one
    } else if (b.signum == 0) {
      a
    } else if (b < one) {
      one
    } else {
      _gcd(b, a % b)
    }
  }
}

trait BigDecimalIsField extends Field[BigDecimal] with BigDecimalIsEuclideanRing {
  def div(a:BigDecimal, b:BigDecimal) = a / b
  def ceil(a:BigDecimal): BigDecimal = a.setScale(0, CEILING)
  def floor(a:BigDecimal): BigDecimal = a.setScale(0, FLOOR)
  def round(a:BigDecimal): BigDecimal = a.setScale(0, HALF_UP)
  def isWhole(a:BigDecimal) = a % 1.0 == 0.0
}

trait BigDecimalIsNRoot extends NRoot[BigDecimal] {
  def nroot(a: BigDecimal, k: Int): BigDecimal = {
    if (a.mc.getPrecision <= 0)
      throw new ArithmeticException("Cannot find the nroot of a BigDecimal with unlimited precision.")
    NRoot.nroot(a, k, a.mc)
  }

  private[this] val two = BigDecimal(2)

  // this is newton's method
  override def sqrt(n: BigDecimal): BigDecimal = {
    var x: BigDecimal = BigDecimal(0, n.mc)
    var y: BigDecimal = BigDecimal(Math.sqrt(n.toDouble), n.mc)
    while (x != y) {
      x = y;
      y = ((n / x) + x) / two
    }
    y
  }

  def log(a:BigDecimal) = spire.math.log(a)

  def fpow(a:BigDecimal, b:BigDecimal) = spire.math.pow(a, b)
}

trait BigDecimalEq extends Eq[BigDecimal] {
  def eqv(x:BigDecimal, y:BigDecimal) = x == y
  override def neqv(x:BigDecimal, y:BigDecimal) = x != y
}

trait BigDecimalOrder extends Order[BigDecimal] with BigDecimalEq {
  override def gt(x: BigDecimal, y: BigDecimal) = x > y
  override def gteqv(x: BigDecimal, y: BigDecimal) = x >= y
  override def lt(x: BigDecimal, y: BigDecimal) = x < y
  override def lteqv(x: BigDecimal, y: BigDecimal) = x <= y
  override def min(x: BigDecimal, y: BigDecimal) = x.min(y)
  override def max(x: BigDecimal, y: BigDecimal) = x.max(y)
  def compare(x: BigDecimal, y: BigDecimal) = x.compare(y)
}

trait BigDecimalIsSigned extends Signed[BigDecimal] {
  def signum(a: BigDecimal): Int = a.signum
  def abs(a: BigDecimal): BigDecimal = a.abs
}

trait BigDecimalIsReal extends BigDecimalOrder with BigDecimalIsSigned {
  def toDouble(x: BigDecimal): Double = x.toDouble
}

trait BigDecimalInstances {
  implicit object BigDecimalAlgebra extends BigDecimalIsField with BigDecimalIsNRoot
  implicit object BigDecimalIsReal extends BigDecimalIsReal
}
