package spire.std

import java.math.BigInteger

import spire.algebra.{EuclideanRing, IsIntegral, MetricSpace, NRoot, Order, Signed}

trait BigIntegerIsEuclideanRing extends EuclideanRing[BigInteger] {
  override def minus(a:BigInteger, b:BigInteger): BigInteger = a subtract b
  def negate(a:BigInteger): BigInteger = a.negate
  def one: BigInteger = BigInteger.ONE
  def plus(a:BigInteger, b:BigInteger): BigInteger = a add b
  override def pow(a:BigInteger, b:Int): BigInteger = a pow b
  override def times(a:BigInteger, b:BigInteger): BigInteger = a multiply b
  def zero: BigInteger = BigInteger.ZERO

  override def fromInt(n: Int): BigInteger = BigInteger.valueOf(n)

  def quot(a:BigInteger, b:BigInteger): BigInteger = a divide b
  def mod(a:BigInteger, b:BigInteger): BigInteger = a remainder b
  override def quotmod(a:BigInteger, b:BigInteger): (BigInteger, BigInteger) = {
    val Array(d, r) = a.divideAndRemainder(b)
    (d, r)
  }
  def gcd(a:BigInteger, b:BigInteger): BigInteger = a.gcd(b)
}

// This is not included in the *Instances trait!
trait BigIntegerIsNRoot extends NRoot[BigInteger] {
  def nroot(a: BigInteger, k: Int): BigInteger = if (a.signum < 0 && k % 2 == 1) {
    nroot(a.negate, k).negate
  } else if (a.signum < 0) {
    throw new ArithmeticException("Cannot find %d-root of negative number." format k)
  } else {
    def findNroot(b: BigInteger, i: Int): BigInteger = if (i < 0) {
      b
    } else {
      val c = b setBit i

      if (((c pow k) compareTo a) <= 0)
        findNroot(c, i - 1)
      else
        findNroot(b, i - 1)
    }

    findNroot(BigInteger.ZERO, a.bitLength - 1)
  }
  def fpow(a:BigInteger, b:BigInteger): BigInteger = spire.math.pow(BigDecimal(a), BigDecimal(b)).bigDecimal.toBigInteger
}

trait BigIntegerOrder extends Order[BigInteger] {
  override def eqv(x:BigInteger, y:BigInteger): Boolean = x equals y
  override def neqv(x:BigInteger, y:BigInteger): Boolean = !(x equals y)
  override def gt(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) > 0
  override def gteqv(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) >= 0
  override def lt(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) < 0
  override def lteqv(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) <= 0
  override def min(x: BigInteger, y: BigInteger): BigInteger = x min y
  override def max(x: BigInteger, y: BigInteger): BigInteger = x max y
  def compare(x: BigInteger, y: BigInteger): Int = x compareTo y
}

trait BigIntegerIsSigned extends Signed[BigInteger] {
  def signum(a: BigInteger): Int = a.signum
  def abs(a: BigInteger): BigInteger = a.abs
}

trait BigIntegerIsReal extends IsIntegral[BigInteger] with BigIntegerOrder with BigIntegerIsSigned with Serializable {
  def toDouble(n: BigInteger): Double = n.doubleValue
  def toBigInt(n: BigInteger): BigInt = n
}

trait BigIntegerIsMetricSpace extends MetricSpace[BigInteger, BigInteger] {
  def distance(v: BigInteger, w: BigInteger): BigInteger = (w subtract v).abs
}

@SerialVersionUID(0L)
class BigIntegerAlgebra extends BigIntegerIsEuclideanRing with BigIntegerIsNRoot with BigIntegerIsMetricSpace with BigIntegerIsReal with Serializable

trait BigIntegerInstances {
  implicit final val BigIntegerAlgebra = new BigIntegerAlgebra
  import spire.math.NumberTag._
  implicit final val BigIntegerTag = new LargeTag[BigInteger](Integral, BigInteger.ZERO)
}
