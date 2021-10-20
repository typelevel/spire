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
import spire.syntax.eq._
import spire.math.Rational
import org.scalacheck.Prop._

class BoundScalaCheckSuite extends munit.ScalaCheckSuite {

  property("Bound equality") {
    forAll { (x: Rational, y: Rational) =>
      (x =!= y) ==> {
        import spire.algebra.Eq

        val eq = Eq[Bound[Rational]]

        eq.eqv(EmptyBound(), EmptyBound()) &&
        eq.eqv(Unbound(), Unbound()) &&
        eq.eqv(Open(x), Open(x)) &&
        !(eq.eqv(Open(x), Open(y))) &&
        eq.eqv(Closed(x), Closed(x)) &&
        !(eq.eqv(Closed(x), Closed(y)))
      }
    }
  }
}
