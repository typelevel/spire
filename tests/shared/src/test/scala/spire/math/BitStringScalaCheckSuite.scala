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

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

import spire.implicits._

class BitStringScalaCheckSuite extends munit.ScalaCheckSuite {
  property("operator mappings") {
    def byOp[A: BitString](n: A, i: Int): List[A] =
      List(n << i, n >>> i, n >> i)

    def byName[A](n: A, i: Int)(implicit bs: BitString[A]): List[A] =
      List(bs.leftShift(n, i), bs.rightShift(n, i), bs.signedRightShift(n, i))

    forAll { (n: Byte, i: Int) =>
      byOp(n, i) == byName(n, i)
    }
  }
}
