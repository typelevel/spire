package spire.math

import scala.annotation.tailrec

object ULong {
  @inline final def apply(n: Long): ULong = new ULong(n)

  final def fromInt(n: Int): ULong = new ULong(n & 0xffffffffL)
  final def fromLong(n: Long): ULong = new ULong(n)

  @inline final val MinValue = ULong(0L)
  @inline final val MaxValue = ULong(-1L)

  @tailrec final private[math] def pow(t:Long, b:Long, e:Long): ULong = {
    if (e == 0L) new ULong(t)
    else if ((e & 1L) == 1L) pow(t * b, b * b, e >>> 1L)
    else pow(t, b * b, e >>> 1L)
  }

  @tailrec final private[math] def gcd(a:ULong, b:ULong): ULong = {
    if (b == new ULong(0L)) a else gcd(b, a % b)
  }
}

class ULong(val signed: Long) extends AnyVal {
  final def toByte: Byte = signed.toByte
  final def toChar: Char = signed.toChar
  final def toShort: Short = signed.toShort
  final def toInt: Int = signed.toInt
  final def toLong: Long = signed

  final def toFloat: Float = if (signed < 0)
    -(Long.MinValue.toFloat) - signed.toFloat
  else
    signed.toFloat

  final def toDouble: Double = if (signed < 0)
    -(Long.MinValue.toDouble) - signed.toDouble
  else
    signed.toDouble

  override final def toString: String = if (this.signed >= 0L)
    this.signed.toString
  else
    (-BigInt(Long.MinValue) * 2 + BigInt(this.signed)).toString // ugh, fixme
  
  final def == (that: ULong): Boolean = this.signed == that.signed
  final def != (that: ULong): Boolean = this.signed != that.signed

  final def <= (that: ULong) = if (this.signed >= 0L)
    this.signed <= that.signed || that.signed < 0L
  else
    that.signed >= this.signed

  final def < (that: ULong) = if (this.signed >= 0L)
    this.signed < that.signed || that.signed < 0L
  else
    that.signed > this.signed

  @inline final def >= (that: ULong) = that <= this
  @inline final def > (that: ULong) = that < this
  
  final def unary_- = ULong(this.signed)

  final def + (that: ULong) = ULong(this.signed + that.signed)
  final def - (that: ULong) = ULong(this.signed - that.signed)
  final def * (that: ULong) = ULong(this.signed * that.signed)

  final def / (that: ULong) = {
    val n: Long = this.signed
    val d: Long = that.signed

    if (d == 0) {
      throw new java.lang.ArithmeticException("/ by zero")
    } else if (d < 0) {
      ULong(if (n >= 0 || n < d) 0 else 1)
    } else {
      val half = n >>> 1
      ULong(((half / d) << 1) + (((half % d) << 1) + (n & 1)) / d)
    }
  }

  final def % (that: ULong) = this - (this / that) * that

  final def /% (that: ULong) = {
    val q = this / that
    (q, this - q * that)
  }

  final def unary_~ = ULong(~this.signed)

  final def << (shift: ULong) = ULong(this.signed << shift.signed)
  final def >> (shift: ULong) = ULong(this.signed >>> (shift.signed & 63))
  final def >>> (shift: ULong) = ULong(this.signed >>> (shift.signed & 63))
  final def & (that: ULong) = ULong(this.signed & that.signed)
  final def | (that: ULong) = ULong(this.signed | that.signed)
  final def ^ (that: ULong) = ULong(this.signed ^ that.signed)

  final def ** (that: ULong) = ULong.pow(1L, this.signed, that.signed)

  final def gcd (that: ULong) = ULong.gcd(this, that)
}
