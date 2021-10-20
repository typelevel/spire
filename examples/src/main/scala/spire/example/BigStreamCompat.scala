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

package spire.example

import scala.collection.{IterableFactory, IterableFactoryDefaults, IterableOps}

trait BigStreamCompat[A] extends IterableOps[A, BigStream, BigStream[A]] with IterableFactoryDefaults[A, BigStream] {
  this: BigStream[A] =>
  override def iterableFactory: IterableFactory[BigStream] = BigStream
}

trait BigStreamCompanionCompat extends IterableFactory[BigStream] {
  def from[A](source: IterableOnce[A]): BigStream[A] =
    source.iterator.foldLeft(BigStream.empty[A])((t, a) => new BigCons(a, t))
}
