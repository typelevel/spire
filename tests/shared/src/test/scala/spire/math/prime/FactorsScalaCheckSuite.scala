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
package math.prime

import spire.implicits._
import spire.laws.arb.safeLong
import spire.math.SafeLong

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import spire.math.ArbitrarySupport._
import Ordinal._
import org.scalacheck.Prop._

class FactorsScalaCheckSuite extends munit.ScalaCheckSuite {

  override def scalaCheckTestParameters =
    if (sys.props.get("java.vm.name").contains("Scala Native"))
      // Native is stupidly slow for this suite
      super.scalaCheckTestParameters.withMinSuccessfulTests(5)
    else super.scalaCheckTestParameters

  implicit val arbitraryFactors: Arbitrary[Factors] =
    Arbitrary(arbitrary[SafeLong].map(n => Factors(n)))

  property("Factors(n).value = n") {
    forAll { (n: Long) =>
      Factors(n).value == n
    }
  }

  property("Factors(n) + Factors(m) = n + m") {
    forAll { (n: Long, m: Long) =>
      (Factors(n) + Factors(m)).value == SafeLong(n) + SafeLong(m)
    }
  }

  property("Factors(n) - Factors(m) = n - m") {
    forAll { (n: Long, m: Long) =>
      (Factors(n) - Factors(m)).value == SafeLong(n) - SafeLong(m)
    }
  }

  property("Factors(n) * Factors(m) = n * m") {
    forAll { (n: Long, m: Long) =>
      (Factors(n) * Factors(m)).value == SafeLong(n) * SafeLong(m)
    }
  }

  property("Factors(n) / Factors(m) = n / m") {
    forAll { (n: Long, nz: NonZero[Long]) =>
      val m = nz.num
      (Factors(n) / Factors(m)).value == SafeLong(n) / SafeLong(m)
    }
  }

  property("Factors(n) % Factors(m) = n % m") {
    forAll { (n: Long, nz: NonZero[Long]) =>
      val m = nz.num
      (Factors(n) % Factors(m)).value == SafeLong(n) % SafeLong(m)
    }
  }

  property("Factors(n) /% Factors(m) = n /% m") {
    forAll { (n: Long, nz: NonZero[Long]) =>
      val m = nz.num
      val (x, y) = Factors(n) /% Factors(m)
      (x.value, y.value) == SafeLong(n) /% SafeLong(m)
    }
  }

  property("Factors(n).pow(k) = n.pow(k)") {
    forAll { (n: Long, k: Sized[Int, _1, _10]) =>
      Factors(n).pow(k.num).value == SafeLong(n).pow(k.num)
    }
  }
}
