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

@SerialVersionUID(0L)
class IntAlgebra extends IntIsEuclideanRing with IntIsNRoot with IntIsReal with Serializable

trait IntInstances {
  final val IntBitString: BitString[Int] = new IntIsBitString

  implicit final def ImplicitIntBitString: IntIsBitString =
    this.IntBitString.asInstanceOf[IntIsBitString]

  final val IntAlgebra: EuclideanRing[Int]
    with NRoot[Int]
    with IsIntegral[Int]
    with TruncatedDivisionCRing[Int]
    with Signed[Int]
    with Order[Int] = new IntAlgebra

  implicit final def ImplicitIntAlgebra: IntAlgebra =
    this.IntAlgebra.asInstanceOf[IntAlgebra]

  import spire.math.NumberTag
  import spire.math.NumberTag._
  implicit final val IntTag: NumberTag[Int] = new BuiltinIntTag[Int](0, Int.MinValue, Int.MaxValue)
}
