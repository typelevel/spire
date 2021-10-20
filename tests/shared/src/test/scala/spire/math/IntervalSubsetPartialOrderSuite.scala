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
package math

import spire.implicits._

class IntervalSubsetPartialOrderSuite extends munit.FunSuite {
  import spire.optional.intervalSubsetPartialOrder._

  import Interval.{closed, point}

  test("Minimal and maximal elements of {[1, 3], [3], [2], [1]} by subset partial order") {
    val intervals = Seq(closed(1, 3), point(3), point(2), point(1))
    assertEquals(intervals.pmin.toSet, Set(point(1), point(2), point(3)))
    assertEquals(intervals.pmax.toSet, Set(closed(1, 3)))
  }
}
