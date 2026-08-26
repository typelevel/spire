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
package std

import spire.algebra.CoordinateSpace
import spire.std.double.*

class SeqCoordinateSpaceSuite extends munit.FunSuite {
  test("CoordinateSpace.seq axis returns expected basis vector, doesn't hang") {
    val space = CoordinateSpace.seq[Double, Vector](4)
    val a = space.axis(1)
    assertEquals(a, Vector(0.0, 1.0, 0.0, 0.0))
  }
}
