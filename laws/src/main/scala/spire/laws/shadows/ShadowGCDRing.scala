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

import spire.algebra.{Eq, GCDRing}

trait ShadowGCDRing[A, S] extends GCDRing[Shadow[A, S]] with ShadowCRing[A, S] {
  import shadowing._
  implicit def A: GCDRing[A]
  implicit def S: GCDRing[S]

  def gcd(x: Shadow[A, S], y: Shadow[A, S])(implicit ev: Eq[Shadow[A, S]]): Shadow[A, S] =
    Shadow(A.gcd(x.a, y.a), checked(S.gcd(x.s, y.s)))

  def lcm(x: Shadow[A, S], y: Shadow[A, S])(implicit ev: Eq[Shadow[A, S]]): Shadow[A, S] =
    Shadow(A.lcm(x.a, y.a), checked(S.lcm(x.s, y.s)))
}
