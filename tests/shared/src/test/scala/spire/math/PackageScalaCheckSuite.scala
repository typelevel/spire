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

import org.scalacheck.Prop._

class PackageScalaCheckSuite extends munit.ScalaCheckSuite {
  test("sanity check") {
    assertEquals(fib(0), BigInt(0))
    assertEquals(fib(1), BigInt(1))
  }

  property("fib(n + 2) = fib(n + 1) + fib(n)") {
    forAll { (n0: Byte) =>
      val n = n0.toLong.abs
      fib(n + 2) == fib(n + 1) + fib(n)
    }
  }

  property("(n + 1)! = n! * (n + 1)") {
    fact(0) == 1
    forAll { (n0: Byte) =>
      val n = n0.toLong.abs + 1
      fact(n + 1) == fact(n) * (n + 1)
    }
  }

  property("choose(n, k) = n!/(k! * (n-k)!)") {
    forAll { (n0: Byte, k0: Byte) =>
      val k = k0.toLong.abs
      val n = n0.toLong.abs
      if (k > n) choose(n, k) == 0
      else if (k == 0 || k == n) choose(n, k) == 1
      else choose(n, k) == fact(n) / (fact(k) * fact(n - k))
    }
  }
}
