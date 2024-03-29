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

import spire.implicits._
import spire.math._

object Graphing {
  def main(args: Array[String]): Unit = {

    val r = 9.0

    // x^2 + y^2 = r^2
    // y^2 = r^2 - x^2

    def genx(x: Interval[Double]): Interval[Double] =
      Interval.point(r ** 2) - x ** 2

    def geny(y: Interval[Double]): Interval[Double] =
      y ** 2

    for (iy <- -10 to 10) {
      for (ix <- -10 to 10) {
        val x = genx(ix.toDouble +/- 0.5)
        val y = geny(iy.toDouble +/- 0.5)
        val c = if (x.intersects(y)) "#" else "."
        print(c)
      }
      println("")
    }
  }
}
