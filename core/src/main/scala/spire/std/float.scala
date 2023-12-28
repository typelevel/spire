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
class FloatAlgebra extends FloatIsField with FloatIsNRoot with FloatIsTrig with FloatIsReal with Serializable

trait FloatInstances {
  final val FloatAlgebra
    : Field[Float] with NRoot[Float] with Trig[Float] with IsRational[Float] with Order[Float] =
    new FloatAlgebra
  import Float._
  import spire.math.NumberTag
  import spire.math.NumberTag._
  implicit final val FloatTag: NumberTag[Float] =
    new BuiltinFloatTag(0f, MinValue, MaxValue, NaN, PositiveInfinity, NegativeInfinity) {
      def isInfinite(a: Float): Boolean = java.lang.Float.isInfinite(a)
      def isNaN(a: Float): Boolean = java.lang.Float.isNaN(a)
    }

  implicit final def ImplicitFloatAlgebra: FloatAlgebra =
    this.FloatAlgebra.asInstanceOf[FloatAlgebra]
}
