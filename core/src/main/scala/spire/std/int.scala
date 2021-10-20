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
  override def minus(a: Int, b: Int): Int = a - b
  def negate(a: Int): Int = -a
  def one: Int = 1
  def plus(a: Int, b: Int): Int = a + b
  override def pow(a: Int, b: Int): Int = spire.math.pow(a, b).toInt
  override def times(a: Int, b: Int): Int = a * b
  def zero: Int = 0

  override def fromInt(n: Int): Int = n

  def euclideanFunction(a: Int): BigInt = BigInt(a).abs
  override def equotmod(a: Int, b: Int): (Int, Int) = spire.math.equotmod(a, b)
  def equot(a: Int, b: Int): Int = spire.math.equot(a, b)
  def emod(a: Int, b: Int): Int = spire.math.emod(a, b)
  def gcd(a: Int, b: Int)(implicit ev: Eq[Int]): Int = spire.math.gcd(a, b).toInt
  def lcm(a: Int, b: Int)(implicit ev: Eq[Int]): Int = spire.math.lcm(a, b).toInt
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
  override def eqv(x: Int, y: Int): Boolean = x == y
  override def neqv(x: Int, y: Int): Boolean = x != y
  override def gt(x: Int, y: Int): Boolean = x > y
  override def gteqv(x: Int, y: Int): Boolean = x >= y
  override def lt(x: Int, y: Int): Boolean = x < y
  override def lteqv(x: Int, y: Int): Boolean = x <= y
  def compare(x: Int, y: Int): Int = if (x < y) -1 else if (x == y) 0 else 1
}

trait IntSigned extends Signed[Int] with IntOrder {
  override def signum(a: Int): Int = java.lang.Integer.signum(a)
  override def abs(a: Int): Int = if (a < 0) -a else a
}

trait IntTruncatedDivision extends TruncatedDivisionCRing[Int] with IntSigned {
  def toBigIntOpt(x: Int): Opt[BigInt] = Opt(BigInt(x))
  def tquot(x: Int, y: Int): Int = x / y
  def tmod(x: Int, y: Int): Int = x % y
}

trait IntIsReal extends IsIntegral[Int] with IntTruncatedDivision {
  def toDouble(n: Int): Double = n.toDouble
  def toBigInt(n: Int): BigInt = BigInt(n)
}

@SerialVersionUID(0L)
class IntIsBitString extends BitString[Int] with Serializable {
  def one: Int = -1
  def zero: Int = 0
  def and(a: Int, b: Int): Int = a & b
  def or(a: Int, b: Int): Int = a | b
  def complement(a: Int): Int = ~a

  def signed: Boolean = true
  def width: Int = 32
  def toHexString(n: Int): String = Integer.toHexString(n)

  def bitCount(n: Int): Int = Integer.bitCount(n)
  def highestOneBit(n: Int): Int = Integer.highestOneBit(n)
  def lowestOneBit(n: Int): Int = Integer.lowestOneBit(n)
  def numberOfLeadingZeros(n: Int): Int = Integer.numberOfLeadingZeros(n)
  def numberOfTrailingZeros(n: Int): Int = Integer.numberOfTrailingZeros(n)

  def leftShift(n: Int, i: Int): Int = n << i
  def rightShift(n: Int, i: Int): Int = n >>> i
  def signedRightShift(n: Int, i: Int): Int = n >> i
  def rotateLeft(n: Int, i: Int): Int = Integer.rotateLeft(n, i)
  def rotateRight(n: Int, i: Int): Int = Integer.rotateRight(n, i)
}

@SerialVersionUID(0L)
class IntAlgebra extends IntIsEuclideanRing with IntIsNRoot with IntIsReal with Serializable

trait IntInstances {
  implicit final val IntBitString: BitString[Int] = new IntIsBitString
  implicit final val IntAlgebra: EuclideanRing[Int]
    with NRoot[Int]
    with IsIntegral[Int]
    with TruncatedDivisionCRing[Int]
    with Signed[Int]
    with Order[Int] = new IntAlgebra
  import spire.math.NumberTag
  import spire.math.NumberTag._
  implicit final val IntTag: NumberTag[Int] = new BuiltinIntTag[Int](0, Int.MinValue, Int.MaxValue)
}
