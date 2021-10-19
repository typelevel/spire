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

import spire.algebra.Eq

trait ShadowEq[A, S] extends Eq[Shadow[A, S]] {
  implicit def A: Eq[A]
  implicit def S: Eq[S]

  override def neqv(x: Shadow[A, S], y: Shadow[A, S]): Boolean = {
    val a = A.neqv(x.a, y.a)
    val s = S.neqv(x.s, y.s)
    assert(a == s)
    a
  }

  def eqv(x: Shadow[A, S], y: Shadow[A, S]): Boolean = {
    val a = A.eqv(x.a, y.a)
    val s = S.eqv(x.s, y.s)
    assert(a == s)
    a
  }
}
