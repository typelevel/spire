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

package spire.laws.shadows

import spire.algebra._

trait ShadowAdditiveAbGroup[A, S] extends AdditiveAbGroup[Shadow[A, S]] with ShadowAdditiveCMonoid[A, S] {
  import shadowing._
  implicit def A: AdditiveAbGroup[A]
  implicit def S: AdditiveAbGroup[S]

  def negate(x: Shadow[A, S]): Shadow[A, S] = Shadow(A.negate(x.a), checked(S.negate(x.s)))

  override def minus(x: Shadow[A, S], y: Shadow[A, S]) =
    Shadow(A.minus(x.a, y.a), checked(S.minus(x.s, y.s)))
}
