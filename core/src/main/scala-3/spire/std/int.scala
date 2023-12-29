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
import java.lang.Integer

trait IntIsEuclideanRing extends EuclideanRing[Int] {
  override inline def minus(a: Int, b: Int): Int = a - b
  inline def negate(a: Int): Int = -a
  inline def one: Int = 1
  inline def plus(a: Int, b: Int): Int = a + b
  override inline def pow(a: Int, b: Int): Int = spire.math.pow(a, b).toInt
  override inline def times(a: Int, b: Int): Int = a * b
  inline def zero: Int = 0

  override def fromInt(n: Int): Int = n

  inline def euclideanFunction(a: Int): BigInt = BigInt(a).abs
  override inline def equotmod(a: Int, b: Int): (Int, Int) = spire.math.equotmod(a, b)
  inline def equot(a: Int, b: Int): Int = spire.math.equot(a, b)
  inline def emod(a: Int, b: Int): Int = spire.math.emod(a, b)
  override inline def gcd(a: Int, b: Int)(implicit ev: Eq[Int]): Int = spire.math.gcd(a, b).toInt
  override inline def lcm(a: Int, b: Int)(implicit ev: Eq[Int]): Int = spire.math.lcm(a, b).toInt
}

// Not included in Instances trait.
trait IntIsNRoot extends NRoot[Int] {
  def nroot(x: Int, n: Int): Int = {
    def findnroot(prev: Int, add: Int): Int = {
      val next = prev | add
      val e = Math.pow(next, n)

      if (e == x || add == 0) {
        next
      } else if (e <= 0 || e > x) {
        findnroot(prev, add >> 1)
      } else {
        findnroot(next, add >> 1)
      }
    }

    findnroot(0, 1 << ((33 - n) / n))
  }

  def log(a: Int): Int = Math.log(a.toDouble).toInt
  def fpow(a: Int, b: Int): Int = Math.pow(a, b).toInt
}

trait IntOrder extends Order[Int] {
  override inline def eqv(x: Int, y: Int): Boolean = x == y
  override inline def neqv(x: Int, y: Int): Boolean = x != y
  override inline def gt(x: Int, y: Int): Boolean = x > y
  override inline def gteqv(x: Int, y: Int): Boolean = x >= y
  override inline def lt(x: Int, y: Int): Boolean = x < y
  override inline def lteqv(x: Int, y: Int): Boolean = x <= y
  inline def compare(x: Int, y: Int): Int = if (x < y) -1 else if (x == y) 0 else 1
}

trait IntSigned extends Signed[Int] with IntOrder {
  override inline def signum(a: Int): Int = java.lang.Integer.signum(a)
  override inline def abs(a: Int): Int = if (a < 0) -a else a
}

trait IntTruncatedDivision extends TruncatedDivisionCRing[Int] with IntSigned {
  def toBigIntOpt(x: Int): Opt[BigInt] = Opt(BigInt(x))
  inline def tquot(x: Int, y: Int): Int = x / y
  inline def tmod(x: Int, y: Int): Int = x % y
}

trait IntIsReal extends IsIntegral[Int] with IntTruncatedDivision {
  inline def order = this
  def toDouble(n: Int): Double = n.toDouble
  def toBigInt(n: Int): BigInt = BigInt(n)
}

@SerialVersionUID(0L)
class IntIsBitString extends BitString[Int] with Serializable {
  inline def one: Int = -1
  inline def zero: Int = 0
  inline def and(a: Int, b: Int): Int = a & b
  inline def or(a: Int, b: Int): Int = a | b
  inline def complement(a: Int): Int = ~a

  inline def signed: Boolean = true
  inline def width: Int = 32
  inline def toHexString(n: Int): String = Integer.toHexString(n)

  inline def bitCount(n: Int): Int = Integer.bitCount(n)
  inline def highestOneBit(n: Int): Int = Integer.highestOneBit(n)
  inline def lowestOneBit(n: Int): Int = Integer.lowestOneBit(n)
  inline def numberOfLeadingZeros(n: Int): Int = Integer.numberOfLeadingZeros(n)
  inline def numberOfTrailingZeros(n: Int): Int = Integer.numberOfTrailingZeros(n)

  inline def leftShift(n: Int, i: Int): Int = n << i
  inline def rightShift(n: Int, i: Int): Int = n >>> i
  inline def signedRightShift(n: Int, i: Int): Int = n >> i
  inline def rotateLeft(n: Int, i: Int): Int = Integer.rotateLeft(n, i)
  inline def rotateRight(n: Int, i: Int): Int = Integer.rotateRight(n, i)
}
