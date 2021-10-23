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
package laws

import spire.algebra._
import spire.implicits._

import scala.annotation.nowarn

trait ModuleToRing[V, A] {
  @nowarn
  implicit def ringFromLeftModule(implicit v: LeftModule[V, A], ev: NoImplicit[CModule[V, A]]): Ring[A] = v.scalar
  @nowarn
  implicit def ringFromRightModule(implicit v: RightModule[V, A], ev: NoImplicit[CModule[V, A]]): Ring[A] = v.scalar
}
