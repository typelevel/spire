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
package optional

import scala.collection.Seq
import scala.collection.SeqOps
import spire.algebra.{CModule, Eq, Order}
import spire.std.{SeqVectorEq, SeqVectorOrder}
import spire.std.{ArrayVectorEq, ArrayVectorOrder}
import spire.std.MapVectorEq

trait VectorOrderLow {
  implicit def seqEq[A, CC[A] <: SeqOps[A, Seq, CC[A]]](implicit
    A0: Eq[A],
    module: CModule[CC[A], A]
  ): SeqVectorEq[A, CC[A]] = new SeqVectorEq[A, CC[A]]()(A0, module.scalar)

  implicit def arrayEq[@sp(Int, Long, Float, Double) A](implicit
    ev: Eq[A],
    module: CModule[Array[A], A]
  ): ArrayVectorEq[A] =
    new ArrayVectorEq[A]()(ev, module.scalar)

  implicit def mapEq[K, V](implicit V0: Eq[V], module: CModule[Map[K, V], V]): MapVectorEq[K, V] =
    new MapVectorEq[K, V]()(V0, module.scalar)
}

/**
 * This object provides implicit instances of Eq and Order for Seq-likes that will behave like infinite vectors.
 * Essentially all this means is that `Seq(0, 0, 0) === Seq()`, since both are infinite vectors of zeros. Any element
 * not explicitly set is implied to be 0.
 */
object vectorOrder extends VectorOrderLow {
  implicit def seqOrder[A, CC[A] <: SeqOps[A, Seq, CC[A]]](implicit
    A0: Order[A],
    module: CModule[CC[A], A]
  ): SeqVectorOrder[A, CC[A]] = new SeqVectorOrder[A, CC[A]]()(A0, module.scalar)

  implicit def arrayOrder[@sp(Int, Long, Float, Double) A](implicit
    ev: Order[A],
    module: CModule[Array[A], A]
  ): ArrayVectorOrder[A] =
    new ArrayVectorOrder[A]()(ev, module.scalar)
}
