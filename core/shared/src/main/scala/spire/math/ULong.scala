package spire
package math


import spire.algebra.{IsIntegral, Order, Rig, Signed}

object ULong extends ULongInstances {
  @inline final def apply(n: Long): ULong = new ULong(n)

  final def apply(s: String): ULong = fromBigInt(BigInt(s))

  final def fromInt(n: Int): ULong = new ULong(n & 0xffffffffL)
  final def fromLong(n: Long): ULong = new ULong(n)

  final def fromBigInt(n: BigInt): ULong =
    if (n < 0) throw new IllegalArgumentException(s"$n < 0")
    else new ULong(n.toLong)

  implicit def ulongToBigInt(n: ULong): BigInt = n.toBigInt

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

  private[spire] final val LimitAsDouble: Double =
    spire.math.pow(2.0, 64)

  private[spire] final val LimitAsBigInt: BigInt =
    BigInt(1) << 64
}

class ULong(val signed: Long) extends AnyVal {
  final def toByte: Byte = signed.toByte
  final def toChar: Char = signed.toChar
  final def toShort: Short = signed.toShort
  final def toInt: Int = signed.toInt
  final def toLong: Long = signed

  final def toFloat: Float = {
    if (signed < 0) (ULong.LimitAsDouble + signed.toDouble).toFloat
    else signed.toFloat
  }

  // FIXME: it would be nice to write some "real" floating-point code
  // to correctly find the nearest Double.
  final def toDouble: Double =
    toBigInt.toDouble

  final def toBigInt: BigInt =
    if (signed < 0) ULong.LimitAsBigInt + signed
    else BigInt(signed)

  // FIXME: it would be nice to avoid converting to BigInt here
  override final def toString: String =
    if (signed >= 0L) signed.toString
    else (ULong.LimitAsBigInt + signed).toString

  // TODO: replace these with `===` and `=!=`? Don't we get `==` and `!=` for free with value classes?
  final def == (that: ULong): Boolean = this.signed == that.signed
  final def != (that: ULong): Boolean = this.signed != that.signed

  final def <= (that: ULong): Boolean = if (this.signed >= 0L)
    this.signed <= that.signed || that.signed < 0L
  else
    that.signed >= this.signed && that.signed < 0L

  final def < (that: ULong): Boolean = if (this.signed >= 0L)
    this.signed < that.signed || that.signed < 0L
  else
    that.signed > this.signed && that.signed < 0L

  @inline final def >= (that: ULong): Boolean = that <= this
  @inline final def > (that: ULong): Boolean = that < this

  final def unary_- : ULong = ULong(-this.signed)

  final def + (that: ULong): ULong = ULong(this.signed + that.signed)
  final def - (that: ULong): ULong = ULong(this.signed - that.signed)
  final def * (that: ULong): ULong = ULong(this.signed * that.signed)

  final def / (that: ULong): ULong = {
    val n: Long = this.signed
    val d: Long = that.signed

    if (d == 0) {
      throw new java.lang.ArithmeticException("/ by zero")
    } else if (d < 0) {
      ULong(if (n >= 0 || n < d) 0 else 1)
    } else if (n >= 0) {
      ULong(n / d)
    } else {
      val half = n >>> 1
      if (half < d) {
        ULong(1L)
      } else {
        ULong(((half / d) << 1) + (((half % d) << 1) + (n & 1)) / d)
      }
    }
  }

  final def % (that: ULong): ULong = this - (this / that) * that

  final def /% (that: ULong): (ULong, ULong) = {
    val q = this / that
    (q, this - q * that)
  }

  final def unary_~ : ULong = ULong(~this.signed)

  final def << (shift: Int): ULong = ULong(signed << shift)
  final def >> (shift: Int): ULong = ULong(signed >>> shift)
  final def >>> (shift: Int): ULong = ULong(signed >>> shift)
  final def & (that: ULong): ULong = ULong(this.signed & that.signed)
  final def | (that: ULong): ULong = ULong(this.signed | that.signed)
  final def ^ (that: ULong): ULong = ULong(this.signed ^ that.signed)

  final def ** (that: ULong): ULong = ULong.pow(1L, this.signed, that.signed)

  final def gcd (that: ULong): ULong = ULong.gcd(this, that)
}

trait ULongInstances {
  implicit final val ULongAlgebra = new ULongAlgebra
  implicit final val ULongBitString = new ULongBitString
  import spire.math.NumberTag._
  implicit final val ULongTag = new UnsignedIntTag[ULong](ULong.MinValue, ULong.MaxValue)
}

private[math] trait ULongIsRig extends Rig[ULong] {
  def one: ULong = ULong(1)
  def plus(a:ULong, b:ULong): ULong = a + b
  override def pow(a:ULong, b:Int): ULong = {
    if (b < 0)
      throw new IllegalArgumentException("negative exponent: %s" format b)
    a ** ULong(b)
  }
  override def times(a:ULong, b:ULong): ULong = a * b
  def zero: ULong = ULong(0)
}

private[math] trait ULongOrder extends Order[ULong] {
  override def eqv(x:ULong, y:ULong): Boolean = x == y
  override def neqv(x:ULong, y:ULong): Boolean = x != y
  override def gt(x: ULong, y: ULong): Boolean = x > y
  override def gteqv(x: ULong, y: ULong): Boolean = x >= y
  override def lt(x: ULong, y: ULong): Boolean = x < y
  override def lteqv(x: ULong, y: ULong): Boolean = x <= y
  def compare(x: ULong, y: ULong): Int = if (x < y) -1 else if (x > y) 1 else 0
}

@SerialVersionUID(0L)
private[math] class ULongBitString extends BitString[ULong] with Serializable {
  def one: ULong = ULong(-1L)
  def zero: ULong = ULong(0L)
  def and(a: ULong, b: ULong): ULong = a & b
  def or(a: ULong, b: ULong): ULong = a | b
  def complement(a: ULong): ULong = ~a
  override def xor(a: ULong, b: ULong): ULong = a ^ b

  def signed: Boolean = false
  def width: Int = 64
  def toHexString(n: ULong): String = java.lang.Long.toHexString(n.signed)

  def bitCount(n: ULong): Int = java.lang.Long.bitCount(n.signed)
  def highestOneBit(n: ULong): ULong = ULong(java.lang.Long.highestOneBit(n.signed))
  def lowestOneBit(n: ULong): ULong = ULong(java.lang.Long.lowestOneBit(n.signed))
  def numberOfLeadingZeros(n: ULong): Int = java.lang.Long.numberOfLeadingZeros(n.signed)
  def numberOfTrailingZeros(n: ULong): Int = java.lang.Long.numberOfTrailingZeros(n.signed)

  def leftShift(n: ULong, i: Int): ULong = n << i
  def rightShift(n: ULong, i: Int): ULong = n >> i
  def signedRightShift(n: ULong, i: Int): ULong = n >>> i
  def rotateLeft(n: ULong, i: Int): ULong = ULong(java.lang.Long.rotateLeft(n.signed, i))
  def rotateRight(n: ULong, i: Int): ULong = ULong(java.lang.Long.rotateRight(n.signed, i))
}

private[math] trait ULongIsSigned extends Signed[ULong] {
  def signum(a: ULong): Int = java.lang.Long.signum(a.signed) & 1
  def abs(a: ULong): ULong = a
}

private[math] trait ULongIsReal extends IsIntegral[ULong] with ULongOrder with ULongIsSigned {
  def toDouble(n: ULong): Double = n.toDouble
  def toBigInt(n: ULong): BigInt = n.toBigInt
}

@SerialVersionUID(0L)
private[math] class ULongAlgebra extends ULongIsRig with ULongIsReal with Serializable
