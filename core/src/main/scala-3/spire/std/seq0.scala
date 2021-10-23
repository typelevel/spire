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

package spire
package std

import scala.collection.Factory
import scala.collection.SeqOps

import spire.algebra._
import scala.annotation.nowarn
import scala.util.NotGiven

trait SeqInstances0:
  @nowarn
  implicit def SeqCModule[A, CC[A] <: SeqOps[A, Seq, CC[A]]](using
    ring0: CRing[A],
    cbf0: Factory[A, CC[A]],
    ev: NotGiven[VectorSpace[CC[A], A]]
  ): SeqCModule[A, CC[A]] = new SeqCModule[A, CC[A]]

trait SeqInstances1 extends SeqInstances0:
  implicit def SeqVectorSpace[A, CC[A] <: SeqOps[A, Seq, CC[A]]](using
    field0: Field[A],
    cbf0: Factory[A, CC[A]],
    ev: NotGiven[NormedVectorSpace[CC[A], A]]
  ): SeqVectorSpace[A, CC[A]] = new SeqVectorSpace[A, CC[A]]

  implicit def SeqEq[A, CC[A] <: SeqOps[A, Seq, CC[A]]](using A0: Eq[A]): SeqEq[A, CC[A]] =
    new SeqEq[A, CC[A]]
