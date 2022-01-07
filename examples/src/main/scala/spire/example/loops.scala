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

class Loops {
  def nested(): Unit = {
    cfor(0)(_ < 5, _ + 1) { y =>
      cfor(0)(_ < 3, _ + 1) { x =>
        println((x, y))
      }
    }
  }

  def simple(): Unit = {
    cfor(0)(_ < 10, _ + 1) { i => println(i) }
  }

  def simplew(): Unit = {
    var i = 0
    while (i < 10) {
      println(i)
      i += 1
    }
  }

  def simplet(): Unit = {
    @tailrec def loop(i: Int): Unit = {
      if (i < 10) {
        println(i)
        loop(i + 1)
      }
    }
    loop(0)
  }
}
