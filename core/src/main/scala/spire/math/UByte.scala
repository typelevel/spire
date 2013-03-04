package spire.math

import spire.algebra._

object UByte extends UByteInstances {
  @inline final def apply(n: Byte) = new UByte(n)
  @inline final def apply(n: Int) = new UByte(n.toByte)
}

class UByte(val signed: Byte) extends AnyVal {
  def toByte: Byte = signed
  def toChar: Char = (signed & 0xff).toChar
  def toShort: Short = (signed & 0xff).toShort
  def toInt: Int = signed & 0xff
  def toLong: Long = signed & 0xffL
  def toFloat: Float = toInt.toFloat
  def toDouble: Double = toInt.toDouble

  def isValidByte = signed >= 0
  def isValidShort = true
  def isValidChar = true
  def isValidInt = true
  def isValidLong = true

  override def toString: String = toInt.toString
  
  def == (that: Long): Boolean = this.signed == that.toByte
  def == (that: UByte): Boolean = this.signed == that.signed

  def != (that: Long): Boolean = this.signed != that.toByte
  def != (that: UByte): Boolean = this.signed != that.signed

  def <= (that: UByte) = this.toInt <= that.toInt
  def < (that: UByte) = this.toInt < that.toInt
  def >= (that: UByte) = this.toInt >= that.toInt
  def > (that: UByte) = this.toInt > that.toInt
  
  def unary_- = UByte(-this.signed)

  def + (that: UByte) = UByte(this.signed + that.signed)
  def - (that: UByte) = UByte(this.signed - that.signed)
  def * (that: UByte) = UByte(this.signed * that.signed)
  def / (that: UByte) = UByte(this.toInt / that.toInt)
  def % (that: UByte) = UByte(this.toInt % that.toInt)

  def unary_~ = UByte(~this.signed)

  def << (shift: UByte) = UByte(this.signed << shift.signed)
  def >> (shift: UByte) = UByte(this.signed >>> (shift.signed & 7))
  def >>> (shift: UByte) = UByte(this.signed >>> (shift.signed & 7))
  def & (that: UByte) = UByte(this.signed & that.signed)
  def | (that: UByte) = UByte(this.signed | that.signed)
  def ^ (that: UByte) = UByte(this.signed ^ that.signed)

  def ** (that: UByte) = UByte(pow(this.toLong, that.toLong).toInt)
}

trait UByteInstances {
  implicit object UByteAlgebra extends UByteIsRig
  implicit object UByteBooleanAlgebra extends UByteBooleanAlgebra
  implicit object UByteIsReal extends UByteIsReal
}

trait UByteIsRig extends Rig[UByte] {
  def one: UByte = UByte(1)
  def plus(a:UByte, b:UByte): UByte = a + b
  override def pow(a:UByte, b:Int): UByte = {
    if (b < 0)
      throw new IllegalArgumentException("negative exponent: %s" format b)
    a ** UByte(b)
  }
  override def times(a:UByte, b:UByte): UByte = a * b
  def zero: UByte = UByte(0)
}

trait UByteOrder extends Order[UByte] {
  override def eqv(x:UByte, y:UByte) = x == y
  override def neqv(x:UByte, y:UByte) = x != y
  override def gt(x: UByte, y: UByte) = x > y
  override def gteqv(x: UByte, y: UByte) = x >= y
  override def lt(x: UByte, y: UByte) = x < y
  override def lteqv(x: UByte, y: UByte) = x <= y
  def compare(x: UByte, y: UByte) = if (x < y) -1 else if (x > y) 1 else 0
}

trait UByteIsSigned extends Signed[UByte] {
  def signum(a: UByte): Int = if (a == UByte(0)) 0 else 1
  def abs(a: UByte): UByte = a
}

trait UByteIsReal extends IsReal[UByte] with UByteOrder with UByteIsSigned {
  def toDouble(n: UByte): Double = n.toDouble
}

trait UByteBooleanAlgebra extends BooleanAlgebra[UByte] {
  def one: UByte = UByte(-1: Byte)
  def zero: UByte = UByte(0: Byte)
  def and(a: UByte, b: UByte): UByte = a & b
  def or(a: UByte, b: UByte): UByte = a | b
  def complement(a: UByte): UByte = ~a
  override def xor(a: UByte, b: UByte): UByte = a ^ b
}
