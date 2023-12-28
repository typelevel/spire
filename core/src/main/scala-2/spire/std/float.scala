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
  override def minus(a: Float, b: Float): Float = a - b
  def negate(a: Float): Float = -a
  def one: Float = 1.0f
  def plus(a: Float, b: Float): Float = a + b
  override def pow(a: Float, b: Int): Float = Math.pow(a, b).toFloat
  override def times(a: Float, b: Float): Float = a * b
  def zero: Float = 0.0f
  def div(a: Float, b: Float): Float = a / b

  override def fromInt(n: Int): Float = n.toFloat
  override def fromDouble(n: Double): Float = n.toFloat
}

trait FloatIsNRoot extends NRoot[Float] {
  def nroot(a: Float, k: Int): Float = Math.pow(a, 1 / k.toDouble).toFloat
  override def sqrt(a: Float): Float = Math.sqrt(a).toFloat
  def fpow(a: Float, b: Float): Float = Math.pow(a, b).toFloat
}

trait FloatIsTrig extends Trig[Float] {
  def e: Float = Math.E.toFloat
  def pi: Float = Math.PI.toFloat

  def exp(a: Float): Float = Math.exp(a).toFloat
  def expm1(a: Float): Float = Math.expm1(a).toFloat
  def log(a: Float): Float = Math.log(a).toFloat
  def log1p(a: Float): Float = Math.log1p(a).toFloat

  def sin(a: Float): Float = Math.sin(a.toDouble).toFloat
  def cos(a: Float): Float = Math.cos(a.toDouble).toFloat
  def tan(a: Float): Float = Math.tan(a.toDouble).toFloat

  def asin(a: Float): Float = Math.asin(a.toDouble).toFloat
  def acos(a: Float): Float = Math.acos(a.toDouble).toFloat
  def atan(a: Float): Float = Math.atan(a.toDouble).toFloat
  def atan2(y: Float, x: Float): Float = Math.atan2(y.toDouble, x.toDouble).toFloat

  def sinh(x: Float): Float = Math.sinh(x.toDouble).toFloat
  def cosh(x: Float): Float = Math.cosh(x.toDouble).toFloat
  def tanh(x: Float): Float = Math.tanh(x.toDouble).toFloat

  def toRadians(a: Float): Float = (a * 2 * pi) / 360
  def toDegrees(a: Float): Float = (a * 360) / (2 * pi)
}

trait FloatOrder extends Order[Float] {
  override def eqv(x: Float, y: Float): Boolean = x == y
  override def neqv(x: Float, y: Float): Boolean = x != y
  override def gt(x: Float, y: Float): Boolean = x > y
  override def gteqv(x: Float, y: Float): Boolean = x >= y
  override def lt(x: Float, y: Float): Boolean = x < y
  override def lteqv(x: Float, y: Float): Boolean = x <= y
  override def min(x: Float, y: Float): Float = Math.min(x, y)
  override def max(x: Float, y: Float): Float = Math.max(x, y)
  def compare(x: Float, y: Float): Int = java.lang.Float.compare(x, y)
}

trait FloatSigned extends Signed[Float] with FloatOrder {
  override def signum(a: Float): Int = Math.signum(a).toInt
  override def abs(a: Float): Float = if (a < 0.0f) -a else a
}

trait FloatTruncatedDivision extends TruncatedDivisionCRing[Float] with FloatSigned {
  def order = this
  def toBigIntOpt(a: Float): Opt[BigInt] = if (a.isWhole) Opt(BigDecimal(a.toDouble).toBigInt) else Opt.empty[BigInt]
  def tquot(a: Float, b: Float): Float = (a - (a % b)) / b
  def tmod(a: Float, b: Float): Float = a % b
}

trait FloatIsReal extends IsRational[Float] with FloatTruncatedDivision {
  def toDouble(x: Float): Double = x.toDouble
  def ceil(a: Float): Float = Math.ceil(a).toFloat
  def floor(a: Float): Float = Math.floor(a).toFloat
  def round(a: Float): Float = spire.math.round(a)
  def isWhole(a: Float): Boolean = a % 1.0 == 0.0
  def toRational(a: Float): Rational = Rational(a)
}



