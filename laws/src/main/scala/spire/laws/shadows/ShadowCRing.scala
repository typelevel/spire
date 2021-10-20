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

import spire.algebra.CRing

trait ShadowCRing[A, S] extends CRing[Shadow[A, S]] with ShadowCRig[A, S] with ShadowCRng[A, S] {
  import shadowing._
  implicit def A: CRing[A]
  implicit def S: CRing[S]

  override def fromInt(n: Int): Shadow[A, S] = Shadow(A.fromInt(n), checked(S.fromInt(n)))
  override def fromBigInt(n: BigInt): Shadow[A, S] = Shadow(A.fromBigInt(n), checked(S.fromBigInt(n)))
}
