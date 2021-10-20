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

package spire.syntax

// For internal use only, to help with cross-compilation
@deprecated
private[spire] trait CforSyntax:
  @deprecated
  private[spire] inline def cfor[A](inline init: A)(inline test: A => Boolean, inline next: A => A)(
    inline body: A => Unit
  ): Unit =
    fastFor.fastFor(init)(test, next)(body)

  @deprecated
  private[spire] inline def cforRange(inline r: Range)(inline body: Int => Unit): Unit =
    fastFor.fastForRange(r)(body)

  @deprecated
  private[spire] inline def cforRange2(inline r1: Range, inline r2: Range)(inline body: (Int, Int) => Unit): Unit =
    fastFor.fastForRange2(r1, r2)(body)
