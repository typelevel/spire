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
package example

import spire.algebra._
import spire.math._
import spire.implicits._

object Gcd {
  def gcd0[A: Integral](x: A, y: A): A =
    if (x.emod(y) === Integral[A].fromInt(0)) y else gcd0(y, x.emod(y))

  def gcd1[A: EuclideanRing: Order](x: A, y: A): A =
    if (x.emod(y) === EuclideanRing[A].zero) y else gcd1(y, x.emod(y))
}

object Pythagoras {
  def distance0[A: Fractional](x: A, y: A): A = (x * x + y * y).sqrt
  def distance1[A: Field: NRoot](x: A, y: A): A = (x * x + y * y).sqrt
}
