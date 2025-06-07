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

import spire.algebra.{Field, IsRational, NRoot, Order, Signed, Trig, TruncatedDivisionCRing}
import spire.math.Rational
import spire.util.Opt

import java.lang.Math

trait DoubleIsField extends Field[Double] {
  override inline def minus(a: Double, b: Double): Double = a - b
  inline def negate(a: Double): Double = -a
  inline def one: Double = 1.0
  inline def plus(a: Double, b: Double): Double = a + b
  override inline def pow(a: Double, b: Int): Double = Math.pow(a, b)
  override inline def times(a: Double, b: Double): Double = a * b
  inline def zero: Double = 0.0
  inline def div(a: Double, b: Double): Double = a / b

  override def fromInt(n: Int): Double = n
  override def fromDouble(n: Double): Double = n
}

trait DoubleIsNRoot extends NRoot[Double] {
  inline def nroot(a: Double, k: Int): Double = Math.pow(a, 1 / k.toDouble)
  override inline def sqrt(a: Double): Double = Math.sqrt(a)
  inline def fpow(a: Double, b: Double): Double = Math.pow(a, b)
}

trait DoubleIsTrig extends Trig[Double] {
  inline def e: Double = Math.E
  inline def pi: Double = Math.PI

  inline def exp(a: Double): Double = Math.exp(a)
  inline def expm1(a: Double): Double = Math.expm1(a)
  inline def log(a: Double): Double = Math.log(a)
  inline def log1p(a: Double): Double = Math.log1p(a)

  inline def sin(a: Double): Double = Math.sin(a)
  inline def cos(a: Double): Double = Math.cos(a)
  inline def tan(a: Double): Double = Math.tan(a)

  inline def asin(a: Double): Double = Math.asin(a)
  inline def acos(a: Double): Double = Math.acos(a)
  inline def atan(a: Double): Double = Math.atan(a)
  inline def atan2(y: Double, x: Double): Double = Math.atan2(y, x)

  inline def sinh(x: Double): Double = Math.sinh(x)
  inline def cosh(x: Double): Double = Math.cosh(x)
  inline def tanh(x: Double): Double = Math.tanh(x)

  inline def toRadians(a: Double): Double = (a * 2 * pi) / 360
  inline def toDegrees(a: Double): Double = (a * 360) / (2 * pi)
}

trait DoubleOrder extends Order[Double] {
  override inline def eqv(x: Double, y: Double): Boolean = x == y
  override inline def neqv(x: Double, y: Double): Boolean = x != y
  override inline def gt(x: Double, y: Double): Boolean = x > y
  override inline def gteqv(x: Double, y: Double): Boolean = x >= y
  override inline def lt(x: Double, y: Double): Boolean = x < y
  override inline def lteqv(x: Double, y: Double): Boolean = x <= y
  override inline def min(x: Double, y: Double): Double = Math.min(x, y)
  override inline def max(x: Double, y: Double): Double = Math.max(x, y)
  inline def compare(x: Double, y: Double): Int = java.lang.Double.compare(x, y)
}

trait DoubleSigned extends Signed[Double] with DoubleOrder {
  inline def order = this
  override inline def signum(a: Double): Int = Math.signum(a).toInt
  override inline def abs(a: Double): Double = if (a < 0.0) -a else a
}

trait DoubleTruncatedDivision extends TruncatedDivisionCRing[Double] with DoubleSigned {
  def toBigIntOpt(a: Double): Opt[BigInt] = if (a.isWhole) Opt(BigDecimal(a).toBigInt) else Opt.empty[BigInt]
  inline def tquot(a: Double, b: Double): Double = (a - (a % b)) / b
  inline def tmod(a: Double, b: Double): Double = a % b
}

trait DoubleIsReal extends IsRational[Double] with DoubleTruncatedDivision {
  def toDouble(x: Double): Double = x
  inline def ceil(a: Double): Double = Math.ceil(a)
  inline def floor(a: Double): Double = Math.floor(a)
  inline def round(a: Double): Double = spire.math.round(a)
  inline def isWhole(a: Double): Boolean = a % 1.0 == 0.0
  def toRational(a: Double): Rational = Rational(a)
}
