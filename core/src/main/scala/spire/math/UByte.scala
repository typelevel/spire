/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

import spire.algebra.{CRig, IsIntegral, Order, SignedAdditiveCMonoid, TruncatedDivision}
import spire.util.Opt

object UByte extends UByteInstances {
  @inline final def apply(n: Byte): UByte = new UByte(n)
  @inline final def apply(n: Int): UByte = new UByte(n.toByte)

  @inline final def MinValue: UByte = UByte(0)
  @inline final def MaxValue: UByte = UByte(-1)
}

class UByte(val signed: Byte) extends AnyVal with scala.math.ScalaNumericAnyConversions {
  override def toByte: Byte = signed
  override def toChar: Char = (signed & 0xff).toChar
  override def toShort: Short = (signed & 0xff).toShort
  override def toInt: Int = signed & 0xff
  override def toLong: Long = signed & 0xffL
  override def toFloat: Float = toInt.toFloat
  override def toDouble: Double = toInt.toDouble
  def toBigInt: BigInt = BigInt(toInt)

  override def byteValue: Byte = toByte
  override def shortValue: Short = toShort
  override def intValue: Int = toInt
  override def longValue: Long = toLong
  override def floatValue: Float = toFloat
  override def doubleValue: Double = toDouble

  override def isWhole: Boolean = true
  def underlying: Any = signed

  override def isValidByte: Boolean = signed >= 0
  override def isValidShort: Boolean = true
  override def isValidChar: Boolean = true
  override def isValidInt: Boolean = true
  def isValidLong: Boolean = true

  override def toString: String = toInt.toString

  def ==(that: UByte): Boolean = this.signed == that.signed
  def !=(that: UByte): Boolean = this.signed != that.signed

  def ===(that: UByte): Boolean = this.signed == that.signed
  def =!=(that: UByte): Boolean = this.signed != that.signed

  def <=(that: UByte): Boolean = this.toInt <= that.toInt
  def <(that: UByte): Boolean = this.toInt < that.toInt
  def >=(that: UByte): Boolean = this.toInt >= that.toInt
  def >(that: UByte): Boolean = this.toInt > that.toInt

  def unary_- : UByte = UByte(-this.signed)

  def +(that: UByte): UByte = UByte(this.signed + that.signed)
  def -(that: UByte): UByte = UByte(this.signed - that.signed)
  def *(that: UByte): UByte = UByte(this.signed * that.signed)
  def /(that: UByte): UByte = UByte(this.toInt / that.toInt)
  def %(that: UByte): UByte = UByte(this.toInt % that.toInt)

  def unary_~ : UByte = UByte(~this.signed)

  def <<(shift: Int): UByte = UByte((signed & 0xff) << (shift & 7))
  def >>(shift: Int): UByte = UByte((signed & 0xff) >>> (shift & 7))
  def >>>(shift: Int): UByte = UByte((signed & 0xff) >>> (shift & 7))
  def &(that: UByte): UByte = UByte(this.signed & 0xff & (that.signed & 0xff))
  def |(that: UByte): UByte = UByte(this.signed & 0xff | that.signed & 0xff)
  def ^(that: UByte): UByte = UByte(this.signed & 0xff ^ that.signed & 0xff)

  def **(that: UByte): UByte = UByte(pow(this.toLong, that.toLong).toInt)
}

trait UByteInstances {
  implicit final val UByteAlgebra: CRig[UByte]
    with IsIntegral[UByte]
    with TruncatedDivision[UByte]
    with SignedAdditiveCMonoid[UByte]
    with Order[UByte] =
    new UByteAlgebra
  implicit final val UByteBitString: BitString[UByte] = new UByteBitString
  import spire.math.NumberTag._
  implicit final val UByteTag: NumberTag[UByte] = new UnsignedIntTag[UByte](UByte.MinValue, UByte.MaxValue)
}

private[math] trait UByteIsCRig extends CRig[UByte] {
  def one: UByte = UByte(1)
  def plus(a: UByte, b: UByte): UByte = a + b
  override def pow(a: UByte, b: Int): UByte = {
    if (b < 0)
      throw new IllegalArgumentException("negative exponent: %s".format(b))
    a ** UByte(b)
  }
  override def times(a: UByte, b: UByte): UByte = a * b
  def zero: UByte = UByte(0)
}

private[math] trait UByteSigned extends Order[UByte] with SignedAdditiveCMonoid[UByte] {
  def order = this
  override def eqv(x: UByte, y: UByte): Boolean = x == y
  override def neqv(x: UByte, y: UByte): Boolean = x != y
  override def gt(x: UByte, y: UByte): Boolean = x > y
  override def gteqv(x: UByte, y: UByte): Boolean = x >= y
  override def lt(x: UByte, y: UByte): Boolean = x < y
  override def lteqv(x: UByte, y: UByte): Boolean = x <= y
  def compare(x: UByte, y: UByte): Int = if (x < y) -1 else if (x > y) 1 else 0
  def abs(x: UByte): UByte = x
}

private[math] trait UByteTruncatedDivision extends TruncatedDivision[UByte] with UByteSigned {
  def toBigIntOpt(x: UByte): Opt[BigInt] = Opt(x.toBigInt)
  def tquot(x: UByte, y: UByte): UByte = x / y
  def tmod(x: UByte, y: UByte): UByte = x % y
  def fquot(x: UByte, y: UByte): UByte = x / y
  def fmod(x: UByte, y: UByte): UByte = x % y
}

@SerialVersionUID(0L)
private[math] class UByteBitString extends BitString[UByte] with Serializable {
  def one: UByte = UByte(-1: Byte)
  def zero: UByte = UByte(0: Byte)
  def and(a: UByte, b: UByte): UByte = a & b
  def or(a: UByte, b: UByte): UByte = a | b
  def complement(a: UByte): UByte = ~a
  override def xor(a: UByte, b: UByte): UByte = a ^ b

  def signed: Boolean = false
  def width: Int = 8
  def toHexString(n: UByte): String = Integer.toHexString(n.toInt)

  def bitCount(n: UByte): Int = Integer.bitCount(n.toInt)
  def highestOneBit(n: UByte): UByte = UByte(Integer.highestOneBit(n.toInt))
  def lowestOneBit(n: UByte): UByte = UByte(Integer.lowestOneBit(n.toInt))
  def numberOfLeadingZeros(n: UByte): Int = Integer.numberOfLeadingZeros(n.toInt)
  def numberOfTrailingZeros(n: UByte): Int = Integer.numberOfTrailingZeros(n.toInt)

  def leftShift(n: UByte, i: Int): UByte = n << i
  def rightShift(n: UByte, i: Int): UByte = n >> i
  def signedRightShift(n: UByte, i: Int): UByte = n >>> i
  def rotateLeft(n: UByte, i: Int): UByte = {
    val j = i & 7
    n << j | n >>> 8 - j
  }
  def rotateRight(n: UByte, i: Int): UByte = {
    val j = i & 7
    n >>> j | n << 8 - j
  }
}

private[math] trait UByteIsReal extends IsIntegral[UByte] with UByteTruncatedDivision {
  def toDouble(n: UByte): Double = n.toDouble
  def toBigInt(n: UByte): BigInt = n.toBigInt
}

@SerialVersionUID(0L)
private[math] class UByteAlgebra extends UByteIsCRig with UByteIsReal with Serializable
