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
package algebra

/**
 * A vector space is a group `V` that can be multiplied by scalars in `F` that lie in a field. Scalar multiplication
 * must distribute over vector addition
 * {{{
 * (`x *: (v + w) === x *: v + x *: w`) and scalar addition
 * (`(x + y) *: v === x *: v + y *: v`). Scalar multiplication by 1 in `F` is an identity function
 * (`1 *: v === v`). Scalar multiplication is "associative" (`x *: y *: v === (x * y) *: v`).
 * }}}
 */
trait VectorSpace[V, @sp(Int, Long, Float, Double) F] extends Any with CModule[V, F] {
  implicit def scalar: Field[F]

  def divr(v: V, f: F): V = timesl(scalar.reciprocal(f), v)
}

object VectorSpace {
  @inline final def apply[V, @sp(Int, Long, Float, Double) R](implicit V: VectorSpace[V, R]): VectorSpace[V, R] = V
}
