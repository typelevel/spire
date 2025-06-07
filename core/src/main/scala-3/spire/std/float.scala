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

trait FloatIsField extends Field[Float] {
  override inline def minus(a: Float, b: Float): Float = a - b
  inline def negate(a: Float): Float = -a
  inline def one: Float = 1.0f
  inline def plus(a: Float, b: Float): Float = a + b
  override inline def pow(a: Float, b: Int): Float = Math.pow(a, b).toFloat
  override inline def times(a: Float, b: Float): Float = a * b
  inline def zero: Float = 0.0f
  inline def div(a: Float, b: Float): Float = a / b

  override def fromInt(n: Int): Float = n.toFloat
  override def fromDouble(n: Double): Float = n.toFloat
}

trait FloatIsNRoot extends NRoot[Float] {
  inline def nroot(a: Float, k: Int): Float = Math.pow(a, 1 / k.toDouble).toFloat
  override inline def sqrt(a: Float): Float = Math.sqrt(a).toFloat
  inline def fpow(a: Float, b: Float): Float = Math.pow(a, b).toFloat
}

trait FloatIsTrig extends Trig[Float] {
  inline def e: Float = Math.E.toFloat
  inline def pi: Float = Math.PI.toFloat

  inline def exp(a: Float): Float = Math.exp(a).toFloat
  inline def expm1(a: Float): Float = Math.expm1(a).toFloat
  inline def log(a: Float): Float = Math.log(a).toFloat
  inline def log1p(a: Float): Float = Math.log1p(a).toFloat

  inline def sin(a: Float): Float = Math.sin(a.toDouble).toFloat
  inline def cos(a: Float): Float = Math.cos(a.toDouble).toFloat
  inline def tan(a: Float): Float = Math.tan(a.toDouble).toFloat

  inline def asin(a: Float): Float = Math.asin(a.toDouble).toFloat
  inline def acos(a: Float): Float = Math.acos(a.toDouble).toFloat
  inline def atan(a: Float): Float = Math.atan(a.toDouble).toFloat
  inline def atan2(y: Float, x: Float): Float = Math.atan2(y.toDouble, x.toDouble).toFloat

  inline def sinh(x: Float): Float = Math.sinh(x.toDouble).toFloat
  inline def cosh(x: Float): Float = Math.cosh(x.toDouble).toFloat
  inline def tanh(x: Float): Float = Math.tanh(x.toDouble).toFloat

  inline def toRadians(a: Float): Float = (a * 2 * pi) / 360
  inline def toDegrees(a: Float): Float = (a * 360) / (2 * pi)
}

trait FloatOrder extends Order[Float] {
  override inline def eqv(x: Float, y: Float): Boolean = x == y
  override inline def neqv(x: Float, y: Float): Boolean = x != y
  override inline def gt(x: Float, y: Float): Boolean = x > y
  override inline def gteqv(x: Float, y: Float): Boolean = x >= y
  override inline def lt(x: Float, y: Float): Boolean = x < y
  override inline def lteqv(x: Float, y: Float): Boolean = x <= y
  override inline def min(x: Float, y: Float): Float = Math.min(x, y)
  override inline def max(x: Float, y: Float): Float = Math.max(x, y)
  inline def compare(x: Float, y: Float): Int = java.lang.Float.compare(x, y)
}

trait FloatSigned extends Signed[Float] with FloatOrder {
  override inline def signum(a: Float): Int = Math.signum(a).toInt
  override inline def abs(a: Float): Float = if (a < 0.0f) -a else a
}

trait FloatTruncatedDivision extends TruncatedDivisionCRing[Float] with FloatSigned {
  inline def order = this
  inline def tquot(a: Float, b: Float): Float = (a - (a % b)) / b
  inline def tmod(a: Float, b: Float): Float = a % b
  def toBigIntOpt(a: Float): Opt[BigInt] = if (a.isWhole) Opt(BigDecimal(a.toDouble).toBigInt) else Opt.empty[BigInt]
}

trait FloatIsReal extends IsRational[Float] with FloatTruncatedDivision {
  inline def ceil(a: Float): Float = Math.ceil(a).toFloat
  inline def floor(a: Float): Float = Math.floor(a).toFloat
  inline def round(a: Float): Float = spire.math.round(a)
  inline def isWhole(a: Float): Boolean = a % 1.0 == 0.0
  def toRational(a: Float): Rational = Rational(a)
  def toDouble(x: Float): Double = x.toDouble
}
