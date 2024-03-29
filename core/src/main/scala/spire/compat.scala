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

import spire.algebra.{Eq, EuclideanRing, Field, Order, PartialOrder, Ring, Signed}
import spire.math.{ConvertableFrom, ScalaEquivWrapper, ScalaFractionalWrapper}
import spire.math.{ScalaIntegralWrapper, ScalaNumericWrapper, ScalaOrderingWrapper, ScalaPartialOrderingWrapper}

private[spire] trait CompatPriority1 {
  implicit def numeric[A: Ring: ConvertableFrom: Signed: Order]: scala.math.Numeric[A] =
    new ScalaNumericWrapper[A] {
      val order = Order[A]
      val structure = Ring[A]
      val conversions = ConvertableFrom[A]
      val signed = Signed[A]
    }

  implicit def ordering[A: Order]: scala.math.Ordering[A] =
    new ScalaOrderingWrapper[A] {
      val order = Order[A]
    }

  implicit def partialOrdering[A: PartialOrder]: scala.math.PartialOrdering[A] =
    new ScalaPartialOrderingWrapper[A] {
      val partialOrder = PartialOrder[A]
    }

  implicit def equiv[A: Eq]: scala.math.Equiv[A] =
    new ScalaEquivWrapper[A] {
      val eq = Eq[A]
    }
}

private[spire] trait CompatPriority2 extends CompatPriority1 {
  implicit def fractional[A: Field: ConvertableFrom: Signed: Order]: scala.math.Fractional[A] =
    new ScalaFractionalWrapper[A] {
      val order = Order[A]
      val structure = Field[A]
      val conversions = ConvertableFrom[A]
      val signed = Signed[A]
    }
}

private[spire] trait CompatPriority3 extends CompatPriority2 {
  implicit def integral[A: EuclideanRing: ConvertableFrom: Signed: Order]: scala.math.Integral[A] =
    new ScalaIntegralWrapper[A] {
      val order = Order[A]
      val structure = EuclideanRing[A]
      val conversions = ConvertableFrom[A]
      val signed = Signed[A]
    }
}

object compat extends CompatPriority3
