package spire.math

import spire.algebra._

object UInt extends UIntInstances {
  @inline final def apply(n: Int) = new UInt(n)
  @inline final def apply(n: Long) = new UInt(n.toInt)

  @inline final val MinValue = UInt(0)
  @inline final val MaxValue = UInt(-1)
}

class UInt(val signed: Int) extends AnyVal {
  def toByte: Byte = signed.toByte
  def toChar: Char = signed.toChar
  def toShort: Short = signed.toShort
  def toInt: Int = signed
  def toLong: Long = signed & 0xffffffffL
  def toFloat: Float = toLong.toFloat
  def toDouble: Double = toLong.toDouble

  def isValidByte = toInt == toByte
  def isValidShort = toInt == toShort
  def isValidChar = toInt == toChar
  def isValidInt = signed >= 0
  def isValidLong = true

  override def toString: String = toLong.toString
  
  def == (that: Long): Boolean = this.signed == that.toInt
  def == (that: UInt): Boolean = this.signed == that.signed

  def != (that: Long): Boolean = this.signed != that.toInt
  def != (that: UInt): Boolean = this.signed != that.signed

  def <= (that: UInt) = this.toLong <= that.toLong
  def < (that: UInt) = this.toLong < that.toLong
  def >= (that: UInt) = this.toLong >= that.toLong
  def > (that: UInt) = this.toLong > that.toLong
  
  def unary_- = UInt(this.signed)

  def + (that: UInt) = UInt(this.signed + that.signed)
  def - (that: UInt) = UInt(this.signed - that.signed)
  def * (that: UInt) = UInt(this.signed * that.signed)
  def / (that: UInt) = UInt(this.toLong / that.toLong)
  def % (that: UInt) = UInt(this.toLong % that.toLong)

  def unary_~ = UInt(~this.signed)

  def << (shift: Int) = UInt(signed << shift)
  def >> (shift: Int) = UInt(signed >>> shift)
  def >>> (shift: Int) = UInt(signed >>> shift)
  def & (that: UInt) = UInt(this.signed & that.signed)
  def | (that: UInt) = UInt(this.signed | that.signed)
  def ^ (that: UInt) = UInt(this.signed ^ that.signed)

  def ** (that: UInt) = UInt(pow(this.toLong, that.toLong))
}

trait UIntInstances {
  implicit object UIntAlgebra extends UIntIsRig
  implicit object UIntBooleanAlgebra extends UIntBooleanAlgebra
  implicit object UIntIsReal extends UIntIsReal
}

trait UIntIsRig extends Rig[UInt] {
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

trait UIntOrder extends Order[UInt] {
  override def eqv(x:UInt, y:UInt) = x == y
  override def neqv(x:UInt, y:UInt) = x != y
  override def gt(x: UInt, y: UInt) = x > y
  override def gteqv(x: UInt, y: UInt) = x >= y
  override def lt(x: UInt, y: UInt) = x < y
  override def lteqv(x: UInt, y: UInt) = x <= y
  def compare(x: UInt, y: UInt) = if (x < y) -1 else if (x > y) 1 else 0
}

trait UIntBooleanAlgebra extends BooleanAlgebra[UInt] {
  def one: UInt = UInt(-1)
  def zero: UInt = UInt(0)
  def and(a: UInt, b: UInt): UInt = a & b
  def or(a: UInt, b: UInt): UInt = a | b
  def complement(a: UInt): UInt = ~a
  override def xor(a: UInt, b: UInt): UInt = a ^ b
}

trait UIntIsSigned extends Signed[UInt] {
  def signum(a: UInt): Int = if (a == UInt(0)) 0 else 1
  def abs(a: UInt): UInt = a
}

trait UIntIsReal extends IsReal[UInt] with UIntOrder with UIntIsSigned {
  def toDouble(n: UInt): Double = n.toDouble
}
