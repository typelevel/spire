package spire
package std

import spire.algebra.{EuclideanRing, Gcd, IsIntegral, MetricSpace, NRoot, Order, Signed}

trait BigIntIsEuclideanRing extends EuclideanRing[BigInt] with Gcd[BigInt] {
  override def minus(a:BigInt, b:BigInt): BigInt = a - b
  def negate(a:BigInt): BigInt = -a
  val one: BigInt = BigInt(1)
  def plus(a:BigInt, b:BigInt): BigInt = a + b
  override def pow(a:BigInt, b:Int): BigInt = a pow b
  override def times(a:BigInt, b:BigInt): BigInt = a * b
  val zero: BigInt = BigInt(0)

  override def fromInt(n: Int): BigInt = BigInt(n)

  def quot(a:BigInt, b:BigInt) = a / b
  def mod(a:BigInt, b:BigInt) = a % b
  override def quotmod(a:BigInt, b:BigInt) = a /% b
  def lcm(a:BigInt, b:BigInt) = (a / a.gcd(b)) * b
  def gcd(a:BigInt, b:BigInt) = a.gcd(b)
}

// This is not included in the *Instances trait!
trait BigIntIsNRoot extends NRoot[BigInt] {
  def nroot(a: BigInt, k: Int): BigInt = if (a < 0 && k % 2 == 1) {
    -nroot(-a, k)
  } else if (a < 0) {
    throw new ArithmeticException("Cannot find %d-root of negative number." format k)
  } else {
    def findNroot(b: BigInt, i: Int): BigInt = if (i < 0) {
      b
    } else {
      val c = b setBit i

      if ((c pow k) <= a)
        findNroot(c, i - 1)
      else
        findNroot(b, i - 1)
    }

    findNroot(0, a.bitLength - 1)
  }
  def fpow(a:BigInt, b:BigInt): BigInt = spire.math.pow(BigDecimal(a), BigDecimal(b)).toBigInt
}

trait BigIntOrder extends Order[BigInt] {
  override def eqv(x:BigInt, y:BigInt): Boolean = x == y
  override def neqv(x:BigInt, y:BigInt): Boolean = x != y
  override def gt(x: BigInt, y: BigInt): Boolean = x > y
  override def gteqv(x: BigInt, y: BigInt): Boolean = x >= y
  override def lt(x: BigInt, y: BigInt): Boolean = x < y
  override def lteqv(x: BigInt, y: BigInt): Boolean = x <= y
  override def min(x: BigInt, y: BigInt): BigInt = x.min(y)
  override def max(x: BigInt, y: BigInt): BigInt = x.max(y)
  // Scala compareTo has no guarantee to return only -1, 0 or 1, as per Spire's compare contractm
  // so we call Java's compareTo which does
  def compare(x: BigInt, y: BigInt): Int = x.bigInteger.compareTo(y.bigInteger)
}

trait BigIntIsSigned extends Signed[BigInt] {
  def signum(a: BigInt): Int = a.signum
  def abs(a: BigInt): BigInt = a.abs
}

trait BigIntIsReal extends IsIntegral[BigInt] with BigIntOrder with BigIntIsSigned with Serializable {
  def toDouble(n: BigInt): Double = n.toDouble
  def toBigInt(n: BigInt): BigInt = n
}

trait BigIntIsMetricSpace extends MetricSpace[BigInt, BigInt] {
  def distance(v: BigInt, w: BigInt): BigInt = (w - v).abs
}

@SerialVersionUID(0L)
class BigIntAlgebra extends BigIntIsEuclideanRing with BigIntIsNRoot with BigIntIsMetricSpace with BigIntIsReal with Serializable

trait BigIntInstances {
  implicit final val BigIntAlgebra = new BigIntAlgebra
  import spire.math.NumberTag._
  implicit final val BigIntTag = new LargeTag[BigInt](Integral, BigInt(0))
}
