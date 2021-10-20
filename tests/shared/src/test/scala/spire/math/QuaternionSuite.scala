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

package spire.math

import spire.algebra.DivisionRing

class QuaternionSuite extends munit.FunSuite {

  test("Quaternion[Double].fromDouble") {
    assert(DivisionRing[Quaternion[Rational]].fromDouble(0).isZero)
    assert((-DivisionRing[Quaternion[Rational]].fromDouble(-1)).isValidInt)
    assert(DivisionRing[Quaternion[Rational]].fromDouble(1) === DivisionRing[Quaternion[Rational]].one)
  }

}
