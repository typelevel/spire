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
package std

import spire.algebra.{Eq, EuclideanRing, IsIntegral, NRoot, Order, Signed, TruncatedDivisionCRing}
import spire.math.BitString
import spire.util.Opt

import java.lang.Math

trait LongIsEuclideanRing extends EuclideanRing[Long] {
  override inline def minus(a: Long, b: Long): Long = a - b
  inline def negate(a: Long): Long = -a
  inline def one: Long = 1L
  inline def plus(a: Long, b: Long): Long = a + b
  override inline def pow(a: Long, b: Int): Long = spire.math.pow(a, b)
  override inline def times(a: Long, b: Long): Long = a * b
  inline def zero: Long = 0L

  override def fromInt(n: Int): Long = n

  inline def euclideanFunction(a: Long): BigInt = BigInt(a).abs
  override inline def equotmod(a: Long, b: Long): (Long, Long) = spire.math.equotmod(a, b)
  inline def equot(a: Long, b: Long): Long = spire.math.equot(a, b)
  inline def emod(a: Long, b: Long): Long = spire.math.emod(a, b)
  override inline def gcd(a: Long, b: Long)(implicit ev: Eq[Long]): Long = spire.math.gcd(a, b)
  override inline def lcm(a: Long, b: Long)(implicit ev: Eq[Long]): Long = spire.math.lcm(a, b)
}

// Not included in Instances trait!
trait LongIsNRoot extends NRoot[Long] {
  def nroot(x: Long, n: Int): Long = {
    def findnroot(prev: Long, add: Long): Long = {
      val next = prev | add
      val e = spire.math.pow(next, n)

      if (e == x || add == 0) {
        next
      } else if (e <= 0 || e > x) {
        findnroot(prev, add >> 1)
      } else {
        findnroot(next, add >> 1)
      }
    }

    if (n < 1) throw new IllegalArgumentException(s"nroot($n)")
    else if (n == 1) x
    else findnroot(0, 1L << ((65 - n) / n))
  }
  inline def log(a: Long): Long = Math.log(a.toDouble).toLong
  inline def fpow(a: Long, b: Long): Long = spire.math.pow(a, b) // xyz
}

trait LongOrder extends Order[Long] {
  override inline def eqv(x: Long, y: Long): Boolean = x == y
  override inline def neqv(x: Long, y: Long): Boolean = x != y
  override inline def gt(x: Long, y: Long): Boolean = x > y
  override inline def gteqv(x: Long, y: Long): Boolean = x >= y
  override inline def lt(x: Long, y: Long): Boolean = x < y
  override inline def lteqv(x: Long, y: Long): Boolean = x <= y
  inline def compare(x: Long, y: Long): Int = if (x < y) -1 else if (x == y) 0 else 1
}

trait LongSigned extends Signed[Long] with LongOrder {
  inline def order = this
  override inline def signum(a: Long): Int = java.lang.Long.signum(a)
  override inline def abs(a: Long): Long = if (a < 0L) -a else a
}

trait LongTruncatedDivision extends TruncatedDivisionCRing[Long] with LongSigned {
  def toBigIntOpt(x: Long): Opt[BigInt] = Opt(BigInt(x))
  inline def tquot(x: Long, y: Long): Long = x / y
  inline def tmod(x: Long, y: Long): Long = x % y
}

trait LongIsReal extends IsIntegral[Long] with LongTruncatedDivision {
  def toDouble(n: Long): Double = n.toDouble
  def toBigInt(n: Long): BigInt = BigInt(n)
}

@SerialVersionUID(0L)
class LongIsBitString extends BitString[Long] with Serializable {
  inline def one: Long = -1L
  inline def zero: Long = 0L
  inline def and(a: Long, b: Long): Long = a & b
  inline def or(a: Long, b: Long): Long = a | b
  inline def complement(a: Long): Long = ~a
  override inline def xor(a: Long, b: Long): Long = a ^ b

  inline def signed: Boolean = true
  inline def width: Int = 64
  inline def toHexString(n: Long): String = java.lang.Long.toHexString(n)

  inline def bitCount(n: Long): Int = java.lang.Long.bitCount(n)
  inline def highestOneBit(n: Long): Long = java.lang.Long.highestOneBit(n)
  inline def lowestOneBit(n: Long): Long = java.lang.Long.lowestOneBit(n)
  inline def numberOfLeadingZeros(n: Long): Int = java.lang.Long.numberOfLeadingZeros(n)
  inline def numberOfTrailingZeros(n: Long): Int = java.lang.Long.numberOfTrailingZeros(n)

  inline def leftShift(n: Long, i: Int): Long = n << i
  inline def rightShift(n: Long, i: Int): Long = n >> i
  inline def signedRightShift(n: Long, i: Int): Long = n >>> i
  inline def rotateLeft(n: Long, i: Int): Long = java.lang.Long.rotateLeft(n, i)
  inline def rotateRight(n: Long, i: Int): Long = java.lang.Long.rotateRight(n, i)
}


