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
import scala.util.NotGiven

trait ModuleToRing[V, A] {
  implicit def ringFromLeftModule(using v: LeftModule[V, A], ev: NotGiven[CModule[V, A]]): Ring[A] = v.scalar
  implicit def ringFromRightModule(using v: RightModule[V, A], ev: NotGiven[CModule[V, A]]): Ring[A] = v.scalar
}
