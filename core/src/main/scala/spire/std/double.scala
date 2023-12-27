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

@SerialVersionUID(0L)
class DoubleAlgebra extends DoubleIsField with DoubleIsNRoot with DoubleIsTrig with DoubleIsReal with Serializable

trait DoubleInstances {
  implicit final val DoubleAlgebra: DoubleIsField
    with DoubleIsNRoot
    with DoubleIsTrig
    with DoubleIsReal
    with DoubleSigned
    with Serializable = new DoubleAlgebra
  import Double._
  import spire.math.NumberTag
  import spire.math.NumberTag._
  implicit final val DoubleTag: NumberTag[Double] =
    new BuiltinFloatTag(0d, MinValue, MaxValue, NaN, PositiveInfinity, NegativeInfinity) {
      def isInfinite(a: Double): Boolean = java.lang.Double.isInfinite(a)
      def isNaN(a: Double): Boolean = java.lang.Double.isNaN(a)
    }
}
