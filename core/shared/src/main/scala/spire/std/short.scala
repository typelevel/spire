package spire.std

import spire.algebra.{EuclideanRing, IsIntegral, NRoot, Order, Signed}
import spire.math.BitString

trait ShortIsEuclideanRing extends EuclideanRing[Short] {
  override def minus(a:Short, b:Short): Short = (a - b).toShort
  def negate(a:Short): Short = (-a).toShort
  def one: Short = 1.toShort
  def plus(a:Short, b:Short): Short = (a + b).toShort
  override def pow(a: Short, b:Int): Short = Math.pow(a, b).toShort
  override def times(a:Short, b:Short): Short = (a * b).toShort
  def zero: Short = 0.toShort

  override def fromInt(n: Int): Short = n.toShort

  def quot(a: Short, b: Short): Short = (a / b).toShort
  def mod(a: Short, b: Short): Short = (a % b).toShort
  def gcd(a: Short, b: Short): Short = spire.math.gcd(a, b).toShort
}

// Not included in Instances trait.
trait ShortIsNRoot extends NRoot[Short] {
  def nroot(x: Short, n: Int): Short = {
    def findnroot(prev: Int, add: Int): Short = {
      val next = prev | add
      val e = Math.pow(next, n)

      if (e == x || add == 0) {
        next.toShort
      } else if (e <= 0 || e > x) {
        findnroot(prev, add >> 1)
      } else {
        findnroot(next, add >> 1)
      }
    }

    findnroot(0, 1 << ((33 - n) / n))
  }

  def log(a: Short): Short = Math.log(a.toDouble).toShort
  def fpow(a: Short, b: Short): Short = Math.pow(a, b).toShort
}

trait ShortOrder extends Order[Short] {
  override def eqv(x:Short, y:Short): Boolean = x == y
  override def neqv(x:Short, y:Short): Boolean = x != y
  override def gt(x: Short, y: Short): Boolean = x > y
  override def gteqv(x: Short, y: Short): Boolean = x >= y
  override def lt(x: Short, y: Short): Boolean = x < y
  override def lteqv(x: Short, y: Short): Boolean = x <= y
  def compare(x: Short, y: Short): Int = java.lang.Integer.signum((x: Int) - (y: Int))
}

trait ShortIsSigned extends Signed[Short] {
  def signum(a: Short): Int = a
  def abs(a: Short): Short = (if (a < 0) -a else a).toShort
}

trait ShortIsReal extends IsIntegral[Short] with ShortOrder with ShortIsSigned {
  def toDouble(n: Short): Double = n.toDouble
  def toBigInt(n: Short): BigInt = BigInt(n)
}

@SerialVersionUID(0L)
class ShortIsBitString extends BitString[Short] with Serializable {
  def one: Short = (-1: Short)
  def zero: Short = (0: Short)
  def and(a: Short, b: Short): Short = (a & b).toShort
  def or(a: Short, b: Short): Short = (a | b).toShort
  def complement(a: Short): Short = (~a).toShort
  override def xor(a: Short, b: Short): Short = (a ^ b).toShort

  def signed: Boolean = true
  def width: Int = 16
  def toHexString(n: Short): String = Integer.toHexString(n & 0xffff)

  def bitCount(n: Short): Int = Integer.bitCount(n & 0xffff)
  def highestOneBit(n: Short): Short = (Integer.highestOneBit(n & 0xffff) & 0xffff).toShort
  def lowestOneBit(n: Short): Short = (Integer.lowestOneBit(n & 0xffff) & 0xffff).toShort
  def numberOfLeadingZeros(n: Short): Int = Integer.numberOfLeadingZeros(n & 0xffff) - 16
  def numberOfTrailingZeros(n: Short): Int = if (n == 0) 16 else Integer.numberOfTrailingZeros(n & 0xffff)

  def leftShift(n: Short, i: Int): Short = (((n & 0xffff) << (i & 15)) & 0xffff).toShort
  def rightShift(n: Short, i: Int): Short = (((n & 0xffff) >>> (i & 15)) & 0xffff).toShort
  def signedRightShift(n: Short, i: Int): Short = ((n >> (i & 15)) & 0xffff).toShort
  def rotateLeft(n: Short, i: Int): Short = {
    val j = i & 15
    ((((n & 0xffff) << j) | ((n & 0xffff) >>> (16 - j))) & 0xffff).toShort
  }
  def rotateRight(n: Short, i: Int): Short = {
    val j = i & 15
    ((((n & 0xffff) >>> j) | ((n & 0xffff) << (16 - j))) & 0xffff).toShort
  }
}

@SerialVersionUID(0L)
class ShortAlgebra extends ShortIsEuclideanRing with ShortIsReal with Serializable

trait ShortInstances {
  implicit final val ShortBitString = new ShortIsBitString
  implicit final val ShortAlgebra = new ShortAlgebra
  import spire.math.NumberTag._
  implicit final val ShortTag = new BuiltinIntTag[Short](0, Short.MinValue, Short.MaxValue)
}
