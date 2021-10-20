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

package spire.scalacompat

trait ScalaOrderingWrapperCompat[A] extends scala.math.Ordering[A] {
  override def min[U <: A](x: U, y: U): U = if (lt(x, y)) x else y
  override def max[U <: A](x: U, y: U): U = if (gt(x, y)) x else y
}

trait BuilderCompat[-A, +To] extends scala.collection.mutable.Builder[A, To]
