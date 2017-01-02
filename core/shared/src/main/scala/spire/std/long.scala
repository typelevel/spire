package spire
package std

import spire.algebra.{Eq, EuclideanRing, IsIntegral, NRoot, Order, Signed}
import spire.math.BitString

import java.lang.Math

trait LongIsEuclideanRing extends EuclideanRing[Long] {
  override def minus(a:Long, b:Long): Long = a - b
  def negate(a:Long): Long = -a
  def one: Long = 1L
  def plus(a:Long, b:Long): Long = a + b
  override def pow(a: Long, b:Int): Long = spire.math.pow(a, b)
  override def times(a:Long, b:Long): Long = a * b
  def zero: Long = 0L

  override def fromInt(n: Int): Long = n

  def euclideanFunction(a:Long): BigInt = BigInt(a).abs
  def quot(a:Long, b:Long) = a / b
  def mod(a:Long, b:Long) = a % b
  override def gcd(a:Long, b:Long)(implicit ev: Eq[Long]) = spire.math.gcd(a, b)
  override def lcm(a:Long, b:Long)(implicit ev: Eq[Long]) = spire.math.lcm(a, b)
}

// Not included in Instances trait!
trait LongIsNRoot extends NRoot[Long] {
  def nroot(x: Long, n: Int): Long = {
    def findnroot(prev: Long, add: Long): Long = {
      val next = prev | add
      val e = spire.math.pow(next, n)

      if (e == x || add == 0) {
        next
      } else if (e <= 0 || e > x) {
        findnroot(prev, add >> 1)
      } else {
        findnroot(next, add >> 1)
      }
    }

    if (n < 1) throw new IllegalArgumentException(s"nroot($n)")
    else if (n == 1) x
    else findnroot(0, 1L << ((65 - n) / n))
  }
  def log(a:Long): Long = Math.log(a.toDouble).toLong
  def fpow(a:Long, b:Long): Long = spire.math.pow(a, b) // xyz
}

trait LongOrder extends Order[Long] {
  override def eqv(x:Long, y:Long): Boolean = x == y
  override def neqv(x:Long, y:Long): Boolean = x != y
  override def gt(x: Long, y: Long): Boolean = x > y
  override def gteqv(x: Long, y: Long): Boolean = x >= y
  override def lt(x: Long, y: Long): Boolean = x < y
  override def lteqv(x: Long, y: Long): Boolean = x <= y
  def compare(x: Long, y: Long): Int = if (x < y) -1 else if (x == y) 0 else 1
}

trait LongIsSigned extends Signed[Long] {
  def signum(a: Long): Int = java.lang.Long.signum(a)
  def abs(a: Long): Long = if (a < 0L) -a else a
}

trait LongIsReal extends IsIntegral[Long] with LongOrder with LongIsSigned {
  def toDouble(n: Long): Double = n.toDouble
  def toBigInt(n: Long): BigInt = BigInt(n)
}

@SerialVersionUID(0L)
class LongIsBitString extends BitString[Long] with Serializable {
  def one: Long = -1L
  def zero: Long = 0L
  def and(a: Long, b: Long): Long = a & b
  def or(a: Long, b: Long): Long = a | b
  def complement(a: Long): Long = ~a
  override def xor(a: Long, b: Long): Long = a ^ b

  def signed: Boolean = true
  def width: Int = 64
  def toHexString(n: Long): String = java.lang.Long.toHexString(n)

  def bitCount(n: Long): Int = java.lang.Long.bitCount(n)
  def highestOneBit(n: Long): Long = java.lang.Long.highestOneBit(n)
  def lowestOneBit(n: Long): Long = java.lang.Long.lowestOneBit(n)
  def numberOfLeadingZeros(n: Long): Int = java.lang.Long.numberOfLeadingZeros(n)
  def numberOfTrailingZeros(n: Long): Int = java.lang.Long.numberOfTrailingZeros(n)

  def leftShift(n: Long, i: Int): Long = n << i
  def rightShift(n: Long, i: Int): Long = n >> i
  def signedRightShift(n: Long, i: Int): Long = n >>> i
  def rotateLeft(n: Long, i: Int): Long = java.lang.Long.rotateLeft(n, i)
  def rotateRight(n: Long, i: Int): Long = java.lang.Long.rotateRight(n, i)
}

@SerialVersionUID(0L)
class LongAlgebra extends LongIsEuclideanRing with LongIsNRoot with LongIsReal with Serializable

trait LongInstances {
  implicit final val LongBitString = new LongIsBitString
  implicit final val LongAlgebra = new LongAlgebra
  import spire.math.NumberTag._
  implicit final val LongTag = new BuiltinIntTag[Long](0L, Long.MinValue, Long.MaxValue)
}
