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

import spire.laws.arb.ushort
import org.scalacheck.Prop._

class UShortSuite extends munit.ScalaCheckSuite {

  val zero = UShort(0)
  val one = UShort(1)

  property("n >= 0") {
    forAll { (n: UShort) =>
      n >= zero == true
    }
  }

  property("a + b == b + a") {
    forAll { (a: UShort, b: UShort) => a + b == b + a }
  }

  property("a * b == b * a") {
    forAll { (a: Short, b: Short) => a * b == b * a }
  }

  property("(a + b) - b == a") {
    forAll { (a: UShort, b: UShort) => a + b - b == a }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: UShort) =>
      val error =
        try {
          n / zero
          false
        } catch {
          case _: ArithmeticException => true
        }
      error == true
    }
  }

  property("n / 1 == n") {
    forAll { (n: UShort) =>
      n / one == n
      n % one == zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: UShort, d: UShort) =>
      d != zero ==> {
        val q = n / d
        val r = n % d
        q * d + r == n
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: UShort, d: UShort) =>
      d != zero ==> { n / d <= n == true }
    }
  }

  property("n % d < d") {
    forAll { (n: UShort, d: UShort) =>
      d != zero ==> { n % d < d == true }
    }
  }

  property("n + 1 > n") {
    forAll { (n: UShort) =>
      n != UShort.MaxValue ==> { n + one > n == true }
    }
  }

  property("n + (-n) == 0") {
    forAll { (n: UShort) => n + -n == zero }
  }

  property("a < b") {
    forAll { (a: UShort, b: UShort) => a < b == a.toLong < b.toLong }
  }

  property("a <= b") {
    forAll { (a: UShort, b: UShort) => a <= b == a.toLong <= b.toLong }
  }

  property("a > b") {
    forAll { (a: UShort, b: UShort) => a > b == a.toLong > b.toLong }
  }

  property("a >= b") {
    forAll { (a: UShort, b: UShort) => a >= b == a.toLong >= b.toLong }
  }
}
