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

import org.scalacheck.Prop._

class JVMSafeLongScalaCheckSuite extends munit.ScalaCheckSuite {
  def invariant(z: SafeLong): SafeLong = {
    z match {
      case SafeLongLong(_)       => ()
      case SafeLongBigInteger(n) => assertEquals(BigInt(n).isValidLong, false)
    }
    z
  }

  property("x & y") {
    forAll { (x: BigInt, y: BigInt) =>
      assertEquals(invariant(SafeLong(x) & SafeLong(y)), SafeLong(x & y))
      assertEquals(invariant(SafeLong(x) & y), SafeLong(x & y))
      assertEquals(invariant(SafeLong(x) & y.toLong), SafeLong(x & y.toLong))
    }
  }

  property("x | y") {
    forAll { (x: BigInt, y: BigInt) =>
      assertEquals(invariant(SafeLong(x) | SafeLong(y)), SafeLong(x | y))
      assertEquals(invariant(SafeLong(x) & y), SafeLong(x & y))
      assertEquals(invariant(SafeLong(x) & y.toLong), SafeLong(x & y.toLong))
    }
  }

  property("x ^ y") {
    forAll { (x: BigInt, y: BigInt) =>
      assertEquals(invariant(SafeLong(x) ^ SafeLong(y)), SafeLong(x ^ y))
    }
  }

}
