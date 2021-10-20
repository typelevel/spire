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

import cats.kernel.Comparison
import spire.algebra.Order

trait ShadowOrder[A, S] extends ShadowPartialOrder[A, S] with Order[Shadow[A, S]] {
  implicit def A: Order[A]
  implicit def S: Order[S]

  override def eqv(x: Shadow[A, S], y: Shadow[A, S]): Boolean = super[ShadowPartialOrder].eqv(x, y)

  override def partialCompare(x: Shadow[A, S], y: Shadow[A, S]): Double = super[ShadowPartialOrder].partialCompare(x, y)

  def compare(x: Shadow[A, S], y: Shadow[A, S]): Int = {
    val a = A.compare(x.a, y.a)
    val s = S.compare(x.s, y.s)
    assert(a == s)
    a
  }

  override def comparison(x: Shadow[A, S], y: Shadow[A, S]): Comparison = {
    val a = A.comparison(x.a, y.a)
    val s = S.comparison(x.s, y.s)
    assert(a == s)
    a
  }

  override def min(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.min(x.a, y.a), S.min(x.s, y.s))

  override def max(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.max(x.a, y.a), S.max(x.s, y.s))

  override def neqv(x: Shadow[A, S], y: Shadow[A, S]): Boolean = {
    val a = A.neqv(x.a, y.a)
    val s = S.neqv(x.s, y.s)
    assert(a == s)
    a
  }

  override def lteqv(x: Shadow[A, S], y: Shadow[A, S]): Boolean = {
    val a = A.lteqv(x.a, y.a)
    val s = S.lteqv(x.s, y.s)
    assert(a == s)
    a
  }

  override def lt(x: Shadow[A, S], y: Shadow[A, S]): Boolean = {
    val a = A.lt(x.a, y.a)
    val s = S.lt(x.s, y.s)
    assert(a == s)
    a
  }

  override def gteqv(x: Shadow[A, S], y: Shadow[A, S]): Boolean = {
    val a = A.gteqv(x.a, y.a)
    val s = S.gteqv(x.s, y.s)
    assert(a == s)
    a
  }

  override def gt(x: Shadow[A, S], y: Shadow[A, S]): Boolean = {
    val a = A.gt(x.a, y.a)
    val s = S.gt(x.s, y.s)
    assert(a == s)
    a
  }
}
