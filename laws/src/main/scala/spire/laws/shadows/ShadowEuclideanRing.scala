/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
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

import spire.algebra.EuclideanRing

trait ShadowEuclideanRing[A, S] extends EuclideanRing[Shadow[A, S]] with ShadowGCDRing[A, S] {
  import shadowing._
  implicit def A: EuclideanRing[A]
  implicit def S: EuclideanRing[S]

  def euclideanFunction(x: Shadow[A, S]): BigInt = {
    val a = A.euclideanFunction(x.a)
    val s = S.euclideanFunction(x.s)
    assert(a == s)
    a
  }

  def equot(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.equot(x.a, y.a), checked(S.equot(x.s, y.s)))

  override def equotmod(x: Shadow[A, S], y: Shadow[A, S]): (Shadow[A, S], Shadow[A, S]) = {
    val (a1, a2) = A.equotmod(x.a, y.a)
    val (s1, s2) = S.equotmod(x.s, y.s)
    (Shadow(a1, checked(s1)), Shadow(a2, checked(s2)))
  }

  def emod(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.emod(x.a, y.a), S.emod(x.s, y.s))
}
