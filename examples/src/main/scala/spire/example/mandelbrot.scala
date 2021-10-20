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

object MandelbrotDemo {

  /**
   * Compute whether the complex number c stays contained within a radius-2 circle after 'limit' iterations.
   */
  def mandelbrot(c: Complex[Double], limit: Int): Int = {
    @tailrec def loop(z: Complex[Double], n: Int): Int =
      if (n >= limit) n
      else if (z.abs > 2.0) n - 1
      else loop(z * z + c, n + 1)

    loop(c, 1)
  }

  /**
   * Print an ASCII approximation of the 4x4 box from -2-2i to 2+2i.
   */
  def main(args: Array[String]): Unit = {
    val res = 26 // number of iterations to try before including pt in the set
    val rows = if (args.isEmpty) 20 else args(0).toInt // rows to print
    val cols = rows * 2 // cols to print. most fonts are roughly 1:2

    val h = 4.0 / rows // height per pixel character
    val w = 4.0 / cols // width per pixel character
    val x0 = -2.0 // starting x value (x offset)
    val y0 = -2.0 // starting y value (y offset)

    def pt(x: Int, y: Int) = Complex(x * w + x0, y * h + y0)
    def display(s: String, n: Int) = print(Xterm.rainbow(n) + s)

    // render the area in ASCII, using o's and spaces.
    fastFor(0)(_ <= rows, _ + 1) { y =>
      fastFor(0)(_ <= cols, _ + 1) { x =>
        // if n<res, color the pixel accordingly, otherwise then we
        // treat it as being in the set.
        val n = mandelbrot(pt(x, y), res)
        val pixel = if (n == res) " " else "x"
        display(pixel, n)
      }
      println(Xterm.clear())
    }
  }
}

object Xterm {
  // r, g, b should be 0-5
  def color(r: Int, g: Int, b: Int) = "\u001b[38;5;%dm".format(16 + b + (g * 6) + (r * 36))
  def clear() = "\u001b[0m"

  // given things like rgb(0xffcc99) produce things like color(6, 5, 4)
  def rgb(n: Int) = color(scale(n & 0xff0000), scale(n & 0xff00), scale(n & 0xff))
  private def scale(n: Int) = round((n * 6.0) / 255).toInt

  // 0-25 are colors, 26+ is clear
  def rainbow(n: Int) =
    if (n < 6) Xterm.color(5, n, 0)
    else if (n < 11) Xterm.color(10 - n, 5, 0)
    else if (n < 16) Xterm.color(0, 5, n - 10)
    else if (n < 21) Xterm.color(0, 20 - n, 5)
    else if (n < 26) Xterm.color(n - 20, 0, 5)
    else Xterm.clear()
}
