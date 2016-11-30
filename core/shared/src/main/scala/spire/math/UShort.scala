package spire
package math

import spire.algebra.{IsIntegral, Order, Rig, Signed}

object UShort extends UShortInstances {
  @inline final def apply(n: Char): UShort = new UShort(n)
  @inline final def apply(n: Short): UShort = new UShort(n.toChar)
  @inline final def apply(n: Int): UShort = new UShort(n.toChar)

  @inline final def MinValue: UShort = UShort(0)
  @inline final def MaxValue: UShort = UShort(Char.MaxValue)
}

class UShort(val signed: Char) extends AnyVal {
  def toByte: Byte = signed.toByte
  def toChar: Char = signed
  def toShort: Short = signed.toShort
  def toInt: Int = signed.toInt
  def toLong: Long = signed.toLong
  def toFloat: Float = signed.toFloat
  def toDouble: Double = signed.toDouble
  def toBigInt: BigInt = BigInt(toInt)

  def isValidByte: Boolean = signed == toByte
  def isValidShort: Boolean = signed == toShort
  def isValidChar: Boolean = true
  def isValidInt: Boolean = true
  def isValidLong: Boolean = true

  override def toString: String = toInt.toString

  def == (that: UShort): Boolean = this.signed == that.signed
  def != (that: UShort): Boolean = this.signed != that.signed

  def ===(that: UShort): Boolean = this.signed == that.signed
  def =!=(that: UShort): Boolean = this.signed != that.signed

  def <= (that: UShort): Boolean = this.signed <= that.signed
  def < (that: UShort): Boolean = this.signed < that.signed
  def >= (that: UShort): Boolean = this.signed >= that.signed
  def > (that: UShort): Boolean = this.signed > that.signed

  def unary_- : UShort = UShort(-this.signed)

  def + (that: UShort): UShort = UShort(this.signed + that.signed)
  def - (that: UShort): UShort = UShort(this.signed - that.signed)
  def * (that: UShort): UShort = UShort(this.signed * that.signed)
  def / (that: UShort): UShort = UShort(this.signed / that.signed)
  def % (that: UShort): UShort = UShort(this.signed % that.signed)

  def unary_~ : UShort = UShort(~this.signed)

  def << (shift: Int): UShort = UShort((signed & 0xffff) << (shift & 15))
  def >> (shift: Int): UShort = UShort((signed & 0xffff) >>> (shift & 15))
  def >>> (shift: Int): UShort = UShort((signed & 0xffff) >>> (shift & 15))
  def & (that: UShort): UShort = UShort(this.signed & that.signed)
  def | (that: UShort): UShort = UShort(this.signed | that.signed)
  def ^ (that: UShort): UShort = UShort(this.signed ^ that.signed)

  def ** (that: UShort): UShort = UShort(pow(this.toLong, that.toLong).toChar)
}

trait UShortInstances {
  implicit final val UShortAlgebra = new UShortAlgebra
  implicit final val UShortBitString = new UShortBitString
  import spire.math.NumberTag._
  implicit final val UShortTag = new UnsignedIntTag[UShort](UShort.MinValue, UShort.MaxValue)
}

private[math] trait UShortIsCRig extends CRig[UShort] {
  def one: UShort = UShort(1)
  def plus(a:UShort, b:UShort): UShort = a + b
  override def pow(a:UShort, b:Int): UShort = {
    if (b < 0)
      throw new IllegalArgumentException("negative exponent: %s" format b)
    a ** UShort(b)
  }
  override def times(a:UShort, b:UShort): UShort = a * b
  def zero: UShort = UShort(0)
}

private[math] trait UShortOrder extends Order[UShort] {
  override def eqv(x:UShort, y:UShort): Boolean = x == y
  override def neqv(x:UShort, y:UShort): Boolean = x != y
  override def gt(x: UShort, y: UShort): Boolean = x > y
  override def gteqv(x: UShort, y: UShort): Boolean = x >= y
  override def lt(x: UShort, y: UShort): Boolean = x < y
  override def lteqv(x: UShort, y: UShort): Boolean = x <= y
  def compare(x: UShort, y: UShort): Int = if (x < y) -1 else if (x > y) 1 else 0
}

@SerialVersionUID(0L)
private[math] class UShortBitString extends BitString[UShort] with Serializable {
  def one: UShort = UShort(-1: Short)
  def zero: UShort = UShort(0: Short)
  def and(a: UShort, b: UShort): UShort = a & b
  def or(a: UShort, b: UShort): UShort = a | b
  def complement(a: UShort): UShort = ~a
  override def xor(a: UShort, b: UShort): UShort = a ^ b

  def signed: Boolean = false
  def width: Int = 16
  def toHexString(n: UShort): String = Integer.toHexString(n.toInt)

  def bitCount(n: UShort): Int = Integer.bitCount(n.toInt)
  def highestOneBit(n: UShort): UShort = UShort(Integer.highestOneBit(n.toInt))
  def lowestOneBit(n: UShort): UShort = UShort(Integer.lowestOneBit(n.toInt))
  def numberOfLeadingZeros(n: UShort): Int = Integer.numberOfLeadingZeros(n.toInt)
  def numberOfTrailingZeros(n: UShort): Int = Integer.numberOfTrailingZeros(n.toInt)

  def leftShift(n: UShort, i: Int): UShort = n << i
  def rightShift(n: UShort, i: Int): UShort = n >> i
  def signedRightShift(n: UShort, i: Int): UShort = n >>> i
  def rotateLeft(n: UShort, i: Int): UShort = {
    val j = i & 15
    (n << j) | (n >>> (16 - j))
  }
  def rotateRight(n: UShort, i: Int): UShort = {
    val j = i & 15
    (n >>> j) | (n << (16 - j))
  }
}

private[math] trait UShortIsSigned extends Signed[UShort] {
  def signum(a: UShort): Int = java.lang.Integer.signum(a.signed) & 1
  def abs(a: UShort): UShort = a
}

private[math] trait UShortIsReal extends IsIntegral[UShort] with UShortOrder with UShortIsSigned {
  def toDouble(n: UShort): Double = n.toDouble
  def toBigInt(n: UShort): BigInt = n.toBigInt
}

@SerialVersionUID(0L)
private[math] class UShortAlgebra extends UShortIsCRig with UShortIsReal with Serializable
