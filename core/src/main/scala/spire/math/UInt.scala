package spire.math

import spire.algebra.{IsIntegral, Order, Rig, Signed}

object UInt extends UIntInstances {
  @inline final def apply(n: Int): UInt = new UInt(n)
  @inline final def apply(n: Long): UInt = new UInt(n.toInt)

  @inline final val MinValue: UInt = UInt(0)
  @inline final val MaxValue: UInt = UInt(-1)
}

class UInt(val signed: Int) extends AnyVal {
  def toByte: Byte = signed.toByte
  def toChar: Char = signed.toChar
  def toShort: Short = signed.toShort
  def toInt: Int = signed
  def toLong: Long = signed & 0xffffffffL
  def toFloat: Float = toLong.toFloat
  def toDouble: Double = toLong.toDouble
  def toBigInt: BigInt = BigInt(toLong)

  def isValidByte: Boolean = toInt == toByte
  def isValidShort: Boolean = toInt == toShort
  def isValidChar: Boolean = toInt == toChar
  def isValidInt: Boolean = signed >= 0
  def isValidLong: Boolean = true

  override def toString: String = toLong.toString

  def == (that: UInt): Boolean = this.signed == that.signed
  def != (that: UInt): Boolean = this.signed != that.signed

  def <= (that: UInt): Boolean = this.toLong <= that.toLong
  def < (that: UInt): Boolean = this.toLong < that.toLong
  def >= (that: UInt): Boolean = this.toLong >= that.toLong
  def > (that: UInt): Boolean = this.toLong > that.toLong

  def unary_- : UInt = UInt(-this.signed)

  def + (that: UInt): UInt = UInt(this.signed + that.signed)
  def - (that: UInt): UInt = UInt(this.signed - that.signed)
  def * (that: UInt): UInt = UInt(this.signed * that.signed)
  def / (that: UInt): UInt = UInt(this.toLong / that.toLong)
  def % (that: UInt): UInt = UInt(this.toLong % that.toLong)

  def unary_~ : UInt = UInt(~this.signed)

  def << (shift: Int): UInt = UInt(signed << shift)
  def >> (shift: Int): UInt = UInt(signed >>> shift)
  def >>> (shift: Int): UInt = UInt(signed >>> shift)
  def & (that: UInt): UInt = UInt(this.signed & that.signed)
  def | (that: UInt): UInt = UInt(this.signed | that.signed)
  def ^ (that: UInt): UInt = UInt(this.signed ^ that.signed)

  def ** (that: UInt): UInt = UInt(pow(this.toLong, that.toLong))
}

trait UIntInstances {
  implicit final val UIntAlgebra = new UIntAlgebra
  implicit final val UIntBitString = new UIntBitString
  import spire.math.NumberTag._
  implicit final val UIntTag = new UnsignedIntTag[UInt](UInt.MinValue, UInt.MaxValue)
}

private[math] trait UIntIsRig extends Rig[UInt] {
  def one: UInt = UInt(1)
  def plus(a:UInt, b:UInt): UInt = a + b
  override def pow(a:UInt, b:Int): UInt = {
    if (b < 0)
      throw new IllegalArgumentException("negative exponent: %s" format b)
    a ** UInt(b)
  }
  override def times(a:UInt, b:UInt): UInt = a * b
  def zero: UInt = UInt(0)
}

private[math] trait UIntOrder extends Order[UInt] {
  override def eqv(x:UInt, y:UInt): Boolean = x == y
  override def neqv(x:UInt, y:UInt): Boolean = x != y
  override def gt(x: UInt, y: UInt): Boolean = x > y
  override def gteqv(x: UInt, y: UInt): Boolean = x >= y
  override def lt(x: UInt, y: UInt): Boolean = x < y
  override def lteqv(x: UInt, y: UInt): Boolean = x <= y
  def compare(x: UInt, y: UInt): Int = if (x < y) -1 else if (x > y) 1 else 0
}

@SerialVersionUID(0L)
private[math] class UIntBitString extends BitString[UInt] with Serializable {
  def one: UInt = UInt(-1)
  def zero: UInt = UInt(0)
  def and(a: UInt, b: UInt): UInt = a & b
  def or(a: UInt, b: UInt): UInt = a | b
  def complement(a: UInt): UInt = ~a
  override def xor(a: UInt, b: UInt): UInt = a ^ b

  def signed: Boolean = false
  def width: Int = 32
  def toHexString(n: UInt): String = Integer.toHexString(n.signed)

  def bitCount(n: UInt): Int = Integer.bitCount(n.signed)
  def highestOneBit(n: UInt): UInt = UInt(Integer.highestOneBit(n.signed))
  def lowestOneBit(n: UInt): UInt = UInt(Integer.lowestOneBit(n.signed))
  def numberOfLeadingZeros(n: UInt): Int = Integer.numberOfLeadingZeros(n.signed)
  def numberOfTrailingZeros(n: UInt): Int = Integer.numberOfTrailingZeros(n.signed)

  def leftShift(n: UInt, i: Int): UInt = n << i
  def rightShift(n: UInt, i: Int): UInt = n >> i
  def signedRightShift(n: UInt, i: Int): UInt = n >>> i
  def rotateLeft(n: UInt, i: Int): UInt = UInt(Integer.rotateLeft(n.signed, i))
  def rotateRight(n: UInt, i: Int): UInt = UInt(Integer.rotateRight(n.signed, i))
}

private[math] trait UIntIsSigned extends Signed[UInt] {
  def signum(a: UInt): Int = java.lang.Integer.signum(a.signed) & 1
  def abs(a: UInt): UInt = a
}

private[math] trait UIntIsReal extends IsIntegral[UInt] with UIntOrder with UIntIsSigned {
  def toDouble(n: UInt): Double = n.toDouble
  def toBigInt(n: UInt): BigInt = n.toBigInt
}

@SerialVersionUID(0L)
private[math] class UIntAlgebra extends UIntIsRig with UIntIsReal with Serializable
