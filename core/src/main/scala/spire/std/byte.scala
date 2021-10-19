/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
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
package std

import spire.algebra.{Eq, EuclideanRing, IsIntegral, NRoot, Order, Signed, TruncatedDivisionCRing}
import spire.util.Opt
import spire.math.BitString

trait ByteIsEuclideanRing extends EuclideanRing[Byte] {
  override def minus(a: Byte, b: Byte): Byte = (a - b).toByte
  def negate(a: Byte): Byte = (-a).toByte
  def one: Byte = 1.toByte
  def plus(a: Byte, b: Byte): Byte = (a + b).toByte
  override def pow(a: Byte, b: Int): Byte = Math.pow(a, b).toByte
  override def times(a: Byte, b: Byte): Byte = (a * b).toByte
  def zero: Byte = 0.toByte

  override def fromInt(n: Int): Byte = n.toByte

  def euclideanFunction(a: Byte): BigInt = BigInt(a).abs
  override def equotmod(a: Byte, b: Byte): (Byte, Byte) = spire.math.equotmod(a, b)
  def equot(a: Byte, b: Byte): Byte = spire.math.equot(a, b)
  def emod(a: Byte, b: Byte): Byte = spire.math.emod(a, b)
  def gcd(a: Byte, b: Byte)(implicit ev: Eq[Byte]): Byte = spire.math.gcd(a, b).toByte
  def lcm(a: Byte, b: Byte)(implicit ev: Eq[Byte]): Byte = spire.math.lcm(a, b).toByte
}

// Not included in Instances trait.
trait ByteIsNRoot extends NRoot[Byte] {
  def nroot(x: Byte, n: Int): Byte = {
    def findnroot(prev: Int, add: Int): Byte = {
      val next = prev | add
      val e = Math.pow(next, n)

      if (e == x || add == 0) {
        next.toByte
      } else if (e <= 0 || e > x) {
        findnroot(prev, add >> 1)
      } else {
        findnroot(next, add >> 1)
      }
    }

    findnroot(0, 1 << ((33 - n) / n))
  }

  def log(a: Byte): Byte = Math.log(a.toDouble).toByte
  def fpow(a: Byte, b: Byte): Byte = Math.pow(a, b).toByte
}

trait ByteOrder extends Order[Byte] {
  override def eqv(x: Byte, y: Byte): Boolean = x == y
  override def neqv(x: Byte, y: Byte): Boolean = x != y
  override def gt(x: Byte, y: Byte): Boolean = x > y
  override def gteqv(x: Byte, y: Byte): Boolean = x >= y
  override def lt(x: Byte, y: Byte): Boolean = x < y
  override def lteqv(x: Byte, y: Byte): Boolean = x <= y
  def compare(x: Byte, y: Byte): Int = java.lang.Integer.signum((x: Int) - (y: Int))
}

trait ByteSigned extends Signed[Byte] with ByteOrder {
  override def signum(a: Byte): Int = java.lang.Integer.signum(a)
  override def abs(a: Byte): Byte = if (a < 0) (-a).toByte else a
}

trait ByteTruncatedDivision extends TruncatedDivisionCRing[Byte] with ByteSigned {
  def toBigIntOpt(x: Byte): Opt[BigInt] = Opt(BigInt(x))
  def tquot(x: Byte, y: Byte): Byte = (x / y).toByte
  def tmod(x: Byte, y: Byte): Byte = (x % y).toByte
}

trait ByteIsReal extends IsIntegral[Byte] with ByteTruncatedDivision with ByteSigned {
  def toDouble(n: Byte): Double = n.toDouble
  def toBigInt(n: Byte): BigInt = BigInt(n)
}

@SerialVersionUID(0L)
class ByteIsBitString extends BitString[Byte] with Serializable {
  def one: Byte = -1: Byte
  def zero: Byte = 0: Byte
  def and(a: Byte, b: Byte): Byte = (a & b).toByte
  def or(a: Byte, b: Byte): Byte = (a | b).toByte
  def complement(a: Byte): Byte = (~a).toByte
  override def xor(a: Byte, b: Byte): Byte = (a ^ b).toByte

  def signed: Boolean = true
  def width: Int = 8
  def toHexString(n: Byte): String = Integer.toHexString(n & 0xff)

  def bitCount(n: Byte): Int = Integer.bitCount(n & 0xff)
  def highestOneBit(n: Byte): Byte = (Integer.highestOneBit(n & 0xff) & 0xff).toByte
  def lowestOneBit(n: Byte): Byte = (Integer.lowestOneBit(n & 0xff) & 0xff).toByte
  def numberOfLeadingZeros(n: Byte): Int = Integer.numberOfLeadingZeros(n & 0xff) - 24
  def numberOfTrailingZeros(n: Byte): Int = if (n == 0) 8 else Integer.numberOfTrailingZeros(n & 0xff)

  def leftShift(n: Byte, i: Int): Byte = (((n & 0xff) << (i & 7)) & 0xff).toByte
  def rightShift(n: Byte, i: Int): Byte = (((n & 0xff) >>> (i & 7)) & 0xff).toByte
  def signedRightShift(n: Byte, i: Int): Byte = ((n >> (i & 7)) & 0xff).toByte
  def rotateLeft(n: Byte, i: Int): Byte = {
    val j = i & 7
    ((((n & 0xff) << j) | ((n & 0xff) >>> (8 - j))) & 0xff).toByte
  }
  def rotateRight(n: Byte, i: Int): Byte = {
    val j = i & 7
    ((((n & 0xff) >>> j) | ((n & 0xff) << (8 - j))) & 0xff).toByte
  }
}

@SerialVersionUID(0L)
class ByteAlgebra extends ByteIsEuclideanRing with ByteIsReal with Serializable

trait ByteInstances {
  implicit final val ByteBitString: BitString[Byte] = new ByteIsBitString
  implicit final val ByteAlgebra
    : EuclideanRing[Byte] with IsIntegral[Byte] with TruncatedDivisionCRing[Byte] with Signed[Byte] with Order[Byte] =
    new ByteAlgebra
  import spire.math.NumberTag
  import spire.math.NumberTag._
  implicit final val ByteTag: NumberTag[Byte] = new BuiltinIntTag[Byte](0, Byte.MinValue, Byte.MaxValue)
}
