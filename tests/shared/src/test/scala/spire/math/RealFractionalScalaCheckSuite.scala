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

import spire.implicits._
import spire.laws.arb.rational
import spire.laws.gen.realFromLongs

import ArbitrarySupport._
import Ordinal._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

class RealFractionalScalaCheckSuite extends munit.ScalaCheckSuite {
  implicit val realArb: Arbitrary[Real] = Arbitrary(realFromLongs)

  property("x.pow(k).nroot(k) = x") {
    forAll { (x0: Real, k: Sized[Int, _1, _10]) =>
      val x = x0.abs
      x.pow(k.num).nroot(k.num) == x
    }
  }

  property("x.nroot(k).pow(k) = x") {
    forAll { (x0: Real, k: Sized[Int, _1, _10]) =>
      val x = x0.abs
      x.nroot(k.num).pow(k.num) == x
    }
  }

  property("x.nroot(-k).pow(-k) = x") {
    forAll { (x0: NonZero[Real], k: Sized[Int, _1, _10]) =>
      val x = x0.num.abs
      x.nroot(-k.num).pow(-k.num) == x
    }
  }

}
