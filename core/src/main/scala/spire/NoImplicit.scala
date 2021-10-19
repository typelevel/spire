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

package spire

import scala.annotation.nowarn

/**
 * `NoImplicit` provides a way to ensure that a particular implicit doesn't exist. It is often useful to work-around
 * annoying ambiguous implicit problems.
 */
final class NoImplicit[A]

object NoImplicit {
  implicit def noImplicit0[A]: NoImplicit[A] = new NoImplicit[A]
  @nowarn
  implicit def noImplicit1[A](implicit ev: A): NoImplicit[A] = new NoImplicit[A]
}
