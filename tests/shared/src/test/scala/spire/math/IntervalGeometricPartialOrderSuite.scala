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
package math

import spire.implicits._

class IntervalGeometricPartialOrderSuite extends munit.FunSuite {
  import spire.optional.intervalGeometricPartialOrder._

  import Interval.{closed, open, openLower, openUpper, point}
  test("[2, 3) === [2, 3)") { assertEquals(openUpper(2, 3).partialCompare(openUpper(2, 3)), 0.0) }
  test("[2, 3) < [3, 4]") { assert(openUpper(2, 3) < closed(3, 4)) }
  test("[2, 3] < (3, 4]") { assert(closed(2, 3) < openLower(3, 4)) }
  test("[2, 3] cannot be compared to [3, 4]") { assert(closed(2, 3).partialCompare(closed(3, 4)).isNaN) }
  test("[3, 4] > [2, 3)") { assert(closed(3, 4) > openUpper(2, 3)) }
  test("[2, 3) <= [3, 4]") { assert(openUpper(2, 3) <= closed(3, 4)) }
  test("[3, 4] >= [2, 3)") { assert(closed(3, 4) >= openUpper(2, 3)) }
  test("not [2, 3] < [3, 4]") { assert(!(closed(2, 3) < closed(3, 4))) }
  test("not [2, 3] <= [3, 4]") { assert(!(closed(2, 3) <= closed(3, 4))) }
  test("not [3, 4] > [3, 4]") { assert(!(closed(2, 3) > closed(3, 4))) }
  test("not [3, 4] >= [3, 4]") { assert(!(closed(2, 3) >= closed(3, 4))) }
  test("empty.partialCompare(empty) == 0.0") { assertEquals(open(2, 2).partialCompare(open(3, 3)), 0.0) }
  test("empty cannot be compared to [2, 3]") { assert(open(2, 2).partialCompare(closed(2, 3)).isNaN) }
  test("[2, 3] cannot be compared to empty") { assert(closed(2, 3).partialCompare(open(2, 2)).isNaN) }
  test("Minimal and maximal elements of {[1], [2, 3], [2, 4]}") {
    val intervals = Seq(point(1), closed(2, 3), closed(2, 4))
    assertEquals(intervals.pmin.toSet, Set(point(1)))
    assertEquals(intervals.pmax.toSet, Set(closed(2, 3), closed(2, 4)))
  }
}
