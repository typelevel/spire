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

import spire.algebra.{Sign, Signed}

trait ShadowSigned[A, S] extends ShadowOrder[A, S] with Signed[Shadow[A, S]] {
  implicit val shadowing: Shadowing[A, S]
  import shadowing._
  implicit def A: Signed[A]
  implicit def S: Signed[S]

  def signum(x: Shadow[A, S]): Int = {
    val a = A.signum(x.a)
    val s = S.signum(x.s)
    assert(a == s)
    a
  }

  def abs(x: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.abs(x.a), checked(S.abs(x.s)))

  override def sign(x: Shadow[A, S]): Sign = {
    val a = A.sign(x.a)
    val s = S.sign(x.s)
    assert(a == s)
    a
  }

  override def isSignZero(x: Shadow[A, S]): Boolean = {
    val a = A.isSignZero(x.a)
    val s = S.isSignZero(x.s)
    assert(a == s)
    a
  }

  override def isSignPositive(x: Shadow[A, S]): Boolean = {
    val a = A.isSignPositive(x.a)
    val s = S.isSignPositive(x.s)
    assert(a == s)
    a
  }

  override def isSignNegative(x: Shadow[A, S]): Boolean = {
    val a = A.isSignNegative(x.a)
    val s = S.isSignNegative(x.s)
    assert(a == s)
    a
  }

  override def isSignNonZero(x: Shadow[A, S]): Boolean = {
    val a = A.isSignNonZero(x.a)
    val s = S.isSignNonZero(x.s)
    assert(a == s)
    a
  }

  override def isSignNonPositive(x: Shadow[A, S]): Boolean = {
    val a = A.isSignNonPositive(x.a)
    val s = S.isSignNonPositive(x.s)
    assert(a == s)
    a
  }

  override def isSignNonNegative(x: Shadow[A, S]): Boolean = {
    val a = A.isSignNonNegative(x.a)
    val s = S.isSignNonNegative(x.s)
    assert(a == s)
    a
  }
}
