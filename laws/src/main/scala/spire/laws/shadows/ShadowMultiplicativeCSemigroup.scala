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

import spire.algebra.MultiplicativeCSemigroup

trait ShadowMultiplicativeCSemigroup[A, S] extends MultiplicativeCSemigroup[Shadow[A, S]] {
  implicit def A: MultiplicativeCSemigroup[A]
  implicit def S: MultiplicativeCSemigroup[S]
  implicit val shadowing: Shadowing[A, S]
  import shadowing._

  def times(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.times(x.a, y.a), checked(S.times(x.s, y.s)))

  override def pow(x: Shadow[A, S], n: Int): Shadow[A, S] =
    Shadow(A.pow(x.a, n), checked(S.pow(x.s, n)))

  override def tryProduct(xs: IterableOnce[Shadow[A, S]]): Option[Shadow[A, S]] = {
    val seq = xs.iterator.toSeq
    val aO = A.tryProduct(seq.map(_.a))
    val sO = S.tryProduct(seq.map(_.s))
    (aO, sO) match {
      case (Some(a), Some(s)) => Some(Shadow(a, checked(s)))
      case (None, None)       => None
      case _ => throw new IllegalArgumentException("Inconsistent results for trySum between primitive and shadow type")
    }
  }
}
