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

import java.math.BigInteger

import spire.algebra.{Eq, EuclideanRing, IsIntegral, MetricSpace, NRoot, Order, Signed, TruncatedDivisionCRing}
import spire.util.Opt

trait BigIntegerIsEuclideanRing extends EuclideanRing[BigInteger] {
  override def minus(a: BigInteger, b: BigInteger): BigInteger = a.subtract(b)
  def negate(a: BigInteger): BigInteger = a.negate
  def one: BigInteger = BigInteger.ONE
  def plus(a: BigInteger, b: BigInteger): BigInteger = a.add(b)
  override def pow(a: BigInteger, b: Int): BigInteger = a.pow(b)
  override def times(a: BigInteger, b: BigInteger): BigInteger = a.multiply(b)
  def zero: BigInteger = BigInteger.ZERO

  override def fromInt(n: Int): BigInteger = BigInteger.valueOf(n)

  def euclideanFunction(a: BigInteger): BigInt = BigInt(a).abs
  override def equotmod(a: BigInteger, b: BigInteger): (BigInteger, BigInteger) = spire.math.equotmod(a, b)
  def equot(a: BigInteger, b: BigInteger): BigInteger = spire.math.equot(a, b)
  def emod(a: BigInteger, b: BigInteger): BigInteger = spire.math.emod(a, b)
  override def gcd(a: BigInteger, b: BigInteger)(implicit ev: Eq[BigInteger]): BigInteger = a.gcd(b)
  override def lcm(a: BigInteger, b: BigInteger)(implicit ev: Eq[BigInteger]): BigInteger =
    if (a.signum == 0 || b.signum == 0) zero else a.divide(a.gcd(b)).multiply(b)
}

// This is not included in the *Instances trait!
trait BigIntegerIsNRoot extends NRoot[BigInteger] {
  def nroot(a: BigInteger, k: Int): BigInteger = if (a.signum < 0 && k % 2 == 1) {
    nroot(a.negate, k).negate
  } else if (a.signum < 0) {
    throw new ArithmeticException("Cannot find %d-root of negative number.".format(k))
  } else {
    def findNroot(b: BigInteger, i: Int): BigInteger = if (i < 0) {
      b
    } else {
      val c = b.setBit(i)

      if (((c.pow(k)).compareTo(a)) <= 0)
        findNroot(c, i - 1)
      else
        findNroot(b, i - 1)
    }

    if (k == 1)
      a
    else if (k > 1) {
      /* The bit length l of b can be interpreted as pow(2, l - 1) <= b < pow(2, l).
         We thus know that the k-th root of b will satisfy
           nroot(pow(2, l - 1), k) <= nroot(b, k) < nroot(pow(2, l))
         Since the k-th root is equivalent to the (1/k)-th power, we can rewrite this as
           pow(pow(2, l - 1), 1 / k) <= nroot(b, k) < pow(pow(2, l), 1 / k)
         which, by the power laws, is equivalent to
           pow(2, (l - 1) / k) <= nroot(b, k) < pow(2, l / k)
         Thus the k-th root of b will have a bit size of at most l / k.
       */
      findNroot(BigInteger.ZERO, a.bitLength / k)
    } else
      throw new ArithmeticException("Cannot find non-positive %d-root of an integer number.".format(k))
  }
  def fpow(a: BigInteger, b: BigInteger): BigInteger =
    spire.math.pow(BigDecimal(a), BigDecimal(b)).bigDecimal.toBigInteger
}

trait BigIntegerOrder extends Order[BigInteger] {
  override def eqv(x: BigInteger, y: BigInteger): Boolean = x.equals(y)
  override def neqv(x: BigInteger, y: BigInteger): Boolean = !x.equals(y)
  override def gt(x: BigInteger, y: BigInteger): Boolean = (x.compareTo(y)) > 0
  override def gteqv(x: BigInteger, y: BigInteger): Boolean = (x.compareTo(y)) >= 0
  override def lt(x: BigInteger, y: BigInteger): Boolean = (x.compareTo(y)) < 0
  override def lteqv(x: BigInteger, y: BigInteger): Boolean = (x.compareTo(y)) <= 0
  override def min(x: BigInteger, y: BigInteger): BigInteger = x.min(y)
  override def max(x: BigInteger, y: BigInteger): BigInteger = x.max(y)
  def compare(x: BigInteger, y: BigInteger): Int = x.compareTo(y)
}

trait BigIntegerSigned extends Signed[BigInteger] with BigIntegerOrder {
  def order = this
  override def signum(a: BigInteger): Int = a.signum
  override def abs(a: BigInteger): BigInteger = a.abs
}

trait BigIntegerTruncatedDivision extends TruncatedDivisionCRing[BigInteger] with BigIntegerSigned {
  def toBigIntOpt(n: BigInteger): Opt[BigInt] = Opt(BigInt(n))
  def tquot(a: BigInteger, b: BigInteger): BigInteger = a.divide(b)
  def tmod(a: BigInteger, b: BigInteger): BigInteger = a.remainder(b)
  override def tquotmod(a: BigInteger, b: BigInteger): (BigInteger, BigInteger) = {
    val arr = a.divideAndRemainder(b)
    val d = arr(0)
    val r = arr(1)
    (d, r)
  }
}

trait BigIntegerIsReal extends IsIntegral[BigInteger] with BigIntegerTruncatedDivision with Serializable {
  def toDouble(n: BigInteger): Double = n.doubleValue
  def toBigInt(n: BigInteger): BigInt = n
}

trait BigIntegerIsMetricSpace extends MetricSpace[BigInteger, BigInteger] {
  def distance(v: BigInteger, w: BigInteger): BigInteger = w.subtract(v).abs
}

@SerialVersionUID(0L)
class BigIntegerAlgebra
    extends BigIntegerIsEuclideanRing
    with BigIntegerIsNRoot
    with BigIntegerIsMetricSpace
    with BigIntegerIsReal
    with Serializable

trait BigIntegerInstances {
  implicit final val BigIntegerAlgebra: EuclideanRing[BigInteger]
    with NRoot[BigInteger]
    with MetricSpace[BigInteger, BigInteger]
    with IsIntegral[BigInteger]
    with TruncatedDivisionCRing[BigInteger]
    with Signed[BigInteger]
    with Order[BigInteger] = new BigIntegerAlgebra
  import spire.math.NumberTag
  import spire.math.NumberTag._
  implicit final val BigIntegerTag: NumberTag[BigInteger] = new LargeTag[BigInteger](Integral, BigInteger.ZERO)
}
