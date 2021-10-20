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

package spire.math.interval

import spire.laws.arb._
import spire.math.{Interval, Rational}
import spire.math.interval.Overlap._
import spire.syntax.eq._
import org.scalacheck.Prop._

class OverlapScalaCheckSuite extends munit.ScalaCheckSuite {

  property("Overlap equality") {
    forAll { (x: Interval[Rational], y: Interval[Rational]) =>
      (x =!= y) ==> {
        import spire.algebra.Eq

        val eq = Eq[Overlap[Rational]]

        eq.eqv(Equal(), Equal()) &&
        eq.eqv(Disjoint(x, y), Disjoint(x, y)) &&
        !eq.eqv(Disjoint(x, y), Disjoint(y, x)) &&
        eq.eqv(PartialOverlap(x, y), PartialOverlap(x, y)) &&
        !eq.eqv(PartialOverlap(x, y), PartialOverlap(y, x)) &&
        eq.eqv(Subset(x, y), Subset(x, y)) &&
        !eq.eqv(Subset(x, y), Subset(y, x))
      }
    }
  }
}
