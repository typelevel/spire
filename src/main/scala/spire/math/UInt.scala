package spire.math

import java.lang.Math.pow

object UInt {
  @inline implicit final def apply(n: Int) = new UInt(n)
  @inline implicit final def apply(n: Long) = new UInt(n.toInt)

  @inline final def MinValue = UInt(0)
  @inline final def MaxValue = UInt(-1)
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

  def << (shift: UInt) = UInt(this.signed << shift.signed)
  def >> (shift: UInt) = UInt(this.signed >>> (shift.signed & 31))
  def >>> (shift: UInt) = UInt(this.signed >>> (shift.signed & 31))
  def & (that: UInt) = UInt(this.signed & that.signed)
  def | (that: UInt) = UInt(this.signed | that.signed)
  def ^ (that: UInt) = UInt(this.signed ^ that.signed)

  def ** (that: UInt) = UInt(fun.pow(this.toLong, that.toLong))
}
