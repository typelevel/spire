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

import spire.algebra.TruncatedDivision
import spire.util.Opt

trait ShadowTruncatedDivision[A, S] extends TruncatedDivision[Shadow[A, S]] with ShadowSigned[A, S] {
  import shadowing._

  implicit def A: TruncatedDivision[A]
  implicit def S: TruncatedDivision[S]

  /**
   * Returns the integer `a` such that `x = a * one`, if it exists.
   */
  def toBigIntOpt(x: Shadow[A, S]): Opt[BigInt] = A.toBigIntOpt(x.a)

  def tquot(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.tquot(x.a, y.a), checked(S.tquot(x.s, y.s)))

  def tmod(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.tmod(x.a, y.a), checked(S.tmod(x.s, y.s)))

  def fquot(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.fquot(x.a, y.a), checked(S.fquot(x.s, y.s)))

  def fmod(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.fmod(x.a, y.a), checked(S.fmod(x.s, y.s)))
}
