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
package math

import spire.algebra.{Eq, EuclideanRing, Field, Order, PartialOrder, Ring, Signed}

trait ScalaOrderingWrapperCompat[A] extends scala.math.Ordering[A] {
  override def min[U <: A](x: U, y: U): U = if (lt(x, y)) x else y
  override def max[U <: A](x: U, y: U): U = if (gt(x, y)) x else y
}

private[spire] trait ScalaEquivWrapper[A] extends scala.math.Equiv[A] {
  def eq: Eq[A]

  def equiv(x: A, y: A): Boolean = eq.eqv(x, y)
}

private[spire] trait ScalaPartialOrderingWrapper[A] extends scala.math.PartialOrdering[A] {
  def partialOrder: PartialOrder[A]

  def tryCompare(x: A, y: A): Option[Int] = partialOrder.tryCompare(x, y)

  override def equiv(x: A, y: A): Boolean = partialOrder.eqv(x, y)
  override def gt(x: A, y: A): Boolean = partialOrder.gt(x, y)
  override def gteq(x: A, y: A): Boolean = partialOrder.gteqv(x, y)
  override def lt(x: A, y: A): Boolean = partialOrder.lt(x, y)
  override def lteq(x: A, y: A): Boolean = partialOrder.lteqv(x, y)
}

private[spire] trait ScalaOrderingWrapper[A] extends ScalaOrderingWrapperCompat[A] {
  def order: Order[A]

  def compare(x: A, y: A): Int = order.compare(x, y)

  override def equiv(x: A, y: A): Boolean = order.eqv(x, y)
  override def gt(x: A, y: A): Boolean = order.gt(x, y)
  override def gteq(x: A, y: A): Boolean = order.gteqv(x, y)
  override def lt(x: A, y: A): Boolean = order.lt(x, y)
  override def lteq(x: A, y: A): Boolean = order.lteqv(x, y)
}

private[spire] trait ScalaNumericWrapper[A] extends scala.math.Numeric[A] with ScalaOrderingWrapper[A] {
  def structure: Ring[A]
  def conversions: ConvertableFrom[A]
  def signed: Signed[A]
  def order: Order[A]

  def fromInt(x: Int): A = structure.fromInt(x)
  def negate(x: A): A = structure.negate(x)
  def minus(x: A, y: A): A = structure.minus(x, y)
  def plus(x: A, y: A): A = structure.plus(x, y)
  def times(x: A, y: A): A = structure.times(x, y)
  override def zero: A = structure.zero
  override def one: A = structure.one

  def toDouble(x: A): Double = conversions.toDouble(x)
  def toFloat(x: A): Float = conversions.toFloat(x)
  def toInt(x: A): Int = conversions.toInt(x)
  def toLong(x: A): Long = conversions.toLong(x)

  override def signum(x: A): Int = signed.signum(x)
  override def abs(x: A): A = signed.abs(x)

  // this is an abstract method starting in scala 2.13
  def parseString(str: String): Option[A] = None
}

private[spire] trait ScalaFractionalWrapper[A] extends ScalaNumericWrapper[A] with scala.math.Fractional[A] {
  def structure: Field[A]

  def div(x: A, y: A): A = structure.div(x, y)
}

private[spire] trait ScalaIntegralWrapper[A] extends ScalaNumericWrapper[A] with scala.math.Integral[A] {
  def structure: EuclideanRing[A]

  def quot(x: A, y: A): A = structure.equot(x, y)
  def rem(x: A, y: A): A = structure.emod(x, y)
}
