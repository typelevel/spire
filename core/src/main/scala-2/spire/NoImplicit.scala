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

import scala.annotation.nowarn

/**
 * `NotGiven` provides a way to ensure that a particular implicit doesn't exist. It is often useful to work-around
 * annoying ambiguous implicit problems. Only for Scala 2. On Scala 3 `spire.NotGiven` is an alias for
 * `scala.util.NotGiven`.
 */
final class NotGiven[A]

object NotGiven {
  implicit def noImplicit0[A]: NotGiven[A] = new NotGiven[A]
  @nowarn
  implicit def noImplicit1[A](implicit ev: A): NotGiven[A] = new NotGiven[A]
}
