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

import spire.algebra.{AdditiveCSemigroup, AdditiveSemigroup}

trait ShadowAdditiveCSemigroup[A, S] extends AdditiveCSemigroup[Shadow[A, S]] {
  implicit val shadowing: Shadowing[A, S]
  import shadowing._
  implicit def A: AdditiveSemigroup[A]
  implicit def S: AdditiveSemigroup[S]

  def plus(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.plus(x.a, y.a), checked(S.plus(x.s, y.s)))

  override def sumN(x: Shadow[A, S], n: Int): Shadow[A, S] =
    Shadow(A.sumN(x.a, n), checked(S.sumN(x.s, n)))

  override def trySum(xs: IterableOnce[Shadow[A, S]]): Option[Shadow[A, S]] = {
    val seq = xs.iterator.toSeq
    val aO = A.trySum(seq.map(_.a))
    val sO = S.trySum(seq.map(_.s))
    (aO, sO) match {
      case (Some(a), Some(s)) => Some(Shadow(a, checked(s)))
      case (None, None)       => None
      case _ => throw new IllegalArgumentException("Inconsistent results for trySum between primitive and shadow type")
    }
  }
}
