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
package syntax
package std

import spire.algebra.{AdditiveMonoid, Field, Monoid, MultiplicativeMonoid, NRoot, Order, PartialOrder, Signed}
import spire.math.{Natural, Number, QuickSort, SafeLong, Searching, ULong}
import scala.collection.Factory
import spire.syntax.cfor._
import spire.syntax.monoid._
import spire.syntax.field._
import spire.syntax.nroot._
import spire.syntax.signed._

final class LiteralIntOps(val lhs: Int) extends AnyVal {
  def /~(rhs: Int): Int = lhs / rhs
  def /%(rhs: Int): (Int, Int) = (lhs / rhs, lhs % rhs)
  def pow(rhs: Int): Int = Math.pow(lhs, rhs).toInt
  def **(rhs: Int): Int = Math.pow(lhs, rhs).toInt
  def unary_! : BigInt = spire.math.fact(lhs)
  def choose(rhs: Int): BigInt = spire.math.choose(lhs, rhs)
}

final class LiteralLongOps(val lhs: Long) extends AnyVal {
  def /~(rhs: Long): Long = lhs / rhs
  def /%(rhs: Long): (Long, Long) = (lhs / rhs, lhs % rhs)
  def pow(rhs: Long): Long = spire.math.pow(lhs, rhs)
  def **(rhs: Long): Long = spire.math.pow(lhs, rhs)
  def unary_! : BigInt = spire.math.fact(lhs)
  def choose(rhs: Long): BigInt = spire.math.choose(lhs, rhs)
}

final class LiteralDoubleOps(val lhs: Double) extends AnyVal {
  def pow(rhs: Double): Double = spire.math.pow(lhs, rhs)
  def **(rhs: Double): Double = spire.math.pow(lhs, rhs)
}

class LiteralBigIntOps(val lhs: BigInt) extends AnyVal {
  def /~(rhs: BigInt): BigInt = lhs / rhs
  def pow(rhs: BigInt): BigInt = spire.math.pow(lhs, rhs)
  def **(rhs: BigInt): BigInt = spire.math.pow(lhs, rhs)

  def +(rhs: SafeLong): SafeLong = SafeLong(lhs) + rhs
  def *(rhs: SafeLong): SafeLong = SafeLong(lhs) * rhs
  def -(rhs: SafeLong): SafeLong = SafeLong(lhs) - rhs
  def /(rhs: SafeLong): SafeLong = SafeLong(lhs) / rhs
  def /~(rhs: SafeLong): SafeLong = SafeLong(lhs) /~ rhs
  def %(rhs: SafeLong): SafeLong = SafeLong(lhs) % rhs
  def /%(rhs: SafeLong): (SafeLong, SafeLong) = SafeLong(lhs) /% rhs

  def +(rhs: Natural): BigInt = lhs + rhs.toBigInt
  def *(rhs: Natural): BigInt = lhs * rhs.toBigInt
  def -(rhs: Natural): BigInt = lhs - rhs.toBigInt
  def /(rhs: Natural): BigInt = lhs / rhs.toBigInt
  def /~(rhs: Natural): BigInt = lhs / rhs.toBigInt
  def %(rhs: Natural): BigInt = lhs % rhs.toBigInt
  def /%(rhs: Natural): (BigInt, BigInt) = lhs /% rhs.toBigInt

  def +(rhs: ULong): BigInt = lhs + rhs.toBigInt
  def *(rhs: ULong): BigInt = lhs * rhs.toBigInt
  def -(rhs: ULong): BigInt = lhs - rhs.toBigInt
  def /(rhs: ULong): BigInt = lhs / rhs.toBigInt
  def /~(rhs: ULong): BigInt = lhs / rhs.toBigInt
  def %(rhs: ULong): BigInt = lhs % rhs.toBigInt
  def /%(rhs: ULong): (BigInt, BigInt) = lhs /% rhs.toBigInt

  def +(rhs: Number): Number = Number(lhs) + rhs
  def *(rhs: Number): Number = Number(lhs) * rhs
  def -(rhs: Number): Number = Number(lhs) - rhs
  def /(rhs: Number): Number = Number(lhs) / rhs
  def /~(rhs: Number): Number = Number(lhs) / rhs
  def %(rhs: Number): Number = Number(lhs).emod(rhs)
  def /%(rhs: Number): (Number, Number) = Number(lhs).equotmod(rhs)
}
