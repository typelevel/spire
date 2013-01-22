package spire.math

object UByte {
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
