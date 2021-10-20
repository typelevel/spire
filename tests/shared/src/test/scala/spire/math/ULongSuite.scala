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

import spire.laws.arb.ulong
import org.scalacheck.Prop._

class ULongSuite extends munit.ScalaCheckSuite {

  val zero = ULong(0L)
  val one = ULong(1L)

  property("n >= 0") {
    forAll { (n: ULong) => n >= zero == true }
  }

  property("a + b == b + a") {
    forAll { (a: ULong, b: ULong) => a + b == b + a }
  }

  property("a * b == b * a") {
    forAll { (a: ULong, b: ULong) => a * b == b * a }
  }

  property("(a + b) - b == a") {
    forAll { (a: ULong, b: ULong) => (a + b) - b == a }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: ULong) =>
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
    forAll { (n: ULong) =>
      n / one == n
      n % one == zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: ULong, d: ULong) =>
      (d != zero) ==> {
        val q = n / d
        val r = n % d
        q * d + r == n
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: ULong, d: ULong) =>
      (d != zero) ==> {
        n / d <= n == true
      }
    }
  }

  property("n % d < d") {
    forAll { (n: ULong, d: ULong) =>
      (d != zero) ==> {
        n % d < d == true
      }
    }
  }

  property("n + 1 > n") {
    forAll { (n: ULong) =>
      (n != ULong.MaxValue) ==> {
        n + one > n == true
      }
    }
  }

  property("n + (-n) == 0") {
    forAll { (n: ULong) => n + (-n) == zero }
  }

  property("a < b") {
    forAll { (a: ULong, b: ULong) => a < b == a.toBigInt < b.toBigInt }
  }

  property("a <= b") {
    forAll { (a: ULong, b: ULong) => a <= b == a.toBigInt <= b.toBigInt }
  }

  property("a > b") {
    forAll { (a: ULong, b: ULong) => a > b == a.toBigInt > b.toBigInt }
  }

  property("a >= b") {
    forAll { (a: ULong, b: ULong) => a >= b == a.toBigInt >= b.toBigInt }
  }

  property("a.toString = a.toBigInt.toString") {
    forAll { (n: ULong) =>
      n.toString == n.toBigInt.toString
    }
  }

  property("toFloat") {
    forAll { (n: ULong) =>
      n.toFloat == n.toBigInt.toFloat
    }
  }

  property("toDouble") {
    forAll { (n: ULong) =>
      n.toDouble == n.toBigInt.toDouble
    }
  }
}
