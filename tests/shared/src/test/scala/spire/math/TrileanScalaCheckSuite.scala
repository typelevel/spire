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

import spire.laws.arb._
import org.scalacheck.Prop._

class TrileanScalaCheckSuite extends munit.ScalaCheckSuite {

  property("associativity") {
    forAll { (x: Trilean, y: Trilean, z: Trilean) =>
      ((x & y) & z) == (x & (y & z)) &&
      ((x | y) | z) == (x | (y | z))
    }
  }

  property("commutativity") {
    forAll { (x: Trilean, y: Trilean) =>
      (x & y) == (y & x) &&
      (x | y) == (y | x)
    }
  }

  property("absorption") {
    forAll { (x: Trilean, y: Trilean) =>
      (x & (x | y)) == x &&
      (x | (x & y)) == x
    }
  }

  property("identity") {
    forAll { (x: Trilean) =>
      (x & Trilean.True) == x &&
      (x | Trilean.False) == x
    }
  }

  property("distributivity") {
    forAll { (x: Trilean, y: Trilean, z: Trilean) =>
      (x & (y | z)) == ((x & y) | (x & z)) &&
      (x | (y & z)) == ((x | y) & (x | z))
    }
  }

  property("Boolean equivalence") {
    forAll { (x: Boolean, y: Boolean) =>
      val tx = Trilean(x)
      val ty = Trilean(y)
      !tx == Trilean(!x) &&
      !ty == Trilean(!y) &&
      (tx & ty) == Trilean(x & y) &&
      (tx | ty) == Trilean(x | y) &&
      (tx ^ ty) == Trilean(x ^ y)
    }
  }
}
