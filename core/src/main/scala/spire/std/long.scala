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
class LongAlgebra extends LongIsEuclideanRing with LongIsNRoot with LongIsReal with Serializable

trait LongInstances {
  final val LongBitString: BitString[Long] = new LongIsBitString

  implicit final def ImplicitLongBitString: LongIsBitString =
    this.LongBitString.asInstanceOf[LongIsBitString]

  final val LongAlgebra: EuclideanRing[Long]
    with NRoot[Long]
    with IsIntegral[Long]
    with TruncatedDivisionCRing[Long]
    with Signed[Long]
    with Order[Long] = new LongAlgebra

  implicit final def ImplicitLongAlgebra: LongAlgebra =
    this.LongAlgebra.asInstanceOf[LongAlgebra]

  import spire.math.NumberTag
  import spire.math.NumberTag._
  implicit final val LongTag: NumberTag[Long] = new BuiltinIntTag[Long](0L, Long.MinValue, Long.MaxValue)
}
