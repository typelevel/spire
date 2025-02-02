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
package algebra

import java.math.BigInteger

import spire.implicits._

class NRootSuite extends munit.FunSuite {
  def testIntegralNRoot[A: Ring: NRoot: ClassTag]: Unit = {
    val cls = implicitly[ClassTag[A]].runtimeClass.getSimpleName
    test("Integral NRoot (%s)".format(cls)) {
      assertEquals(NRoot[A].nroot(Rig[A].one, 2), Rig[A].one)
      assertEquals(NRoot[A].nroot(Ring[A].fromInt(1234), 2), Ring[A].fromInt(35))
      assertEquals(NRoot[A].nroot(Ring[A].fromInt(912384), 3), Ring[A].fromInt(96))
    }
  }

  testIntegralNRoot[Int]
  testIntegralNRoot[Long]
  testIntegralNRoot[BigInt]
  testIntegralNRoot[BigInteger]

  val DECIMAL1 = new java.math.MathContext(1)

  // Returns the smallest value that can be added/removed from x.
  def eps(x: BigDecimal): BigDecimal =
    x.round(DECIMAL1) * BigDecimal(1, x.mc.getPrecision - 1)

  def checkNRoot(x: BigDecimal, n: Int): Unit = {
    import spire.implicits._

    val y = x.nroot(n)
    val e = eps(y)

    if (x > 0) {
      assert(((y - e) ** n) < x, "expected %s ** %d < %s".format(y - e, n, x))
      assert(((y + e) ** n) > x, "expected %s ** %d > %s".format(y + e, n, x))
    } else {
      assert(((y + e) ** n) < x, "expected %s ** %d < %s".format(y + e, n, x))
      assert(((y - e) ** n) > x, "expected %s ** %d > %s".format(y - e, n, x))
    }
  }

  val HighPrecision = new java.math.MathContext(if (spire.tests.Platform.isNative) 150 else 250)

  val bases = Seq(
    BigDecimal(2),
    BigDecimal(3),
    BigDecimal("3492919288716623419872.99818234", HighPrecision),
    BigDecimal("0.00000000000000000000000000000012345")
  )

  val roots = Seq(2, 3, 6, 9, 23, 53)

  test("BigDecimal NRoot") {
    bases.foreach { x =>
      roots.foreach(checkNRoot(x, _))
    }

    bases.map(-_).foreach { x =>
      roots.filter(_ % 2 == 1).foreach(checkNRoot(x, _))
    }
  }
}
