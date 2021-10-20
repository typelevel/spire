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

import scala.util.Try

import spire.implicits._
import spire.laws.arb.rational

import org.scalacheck.Prop._

class IntervalIteratorScalaCheckSuite extends munit.ScalaCheckSuite {

  property("bounded intervals are ok") {
    forAll { (n1: Rational, n2: Rational, num0: Byte) =>
      val (x, y) = if (n1 <= n2) (n1, n2) else (n2, n1)

      val num = ((num0 & 255) % 13) + 1

      def testEndpoints(interval: Interval[Rational], step: Rational, hasLower: Boolean, hasUpper: Boolean): Boolean = {
        val ns = interval.iterator(step).toSet
        val extra = if (hasLower && hasUpper) 2 else if (hasLower || hasUpper) 1 else 0
        ns(x) == hasLower &&
        ns(y) == hasUpper &&
        ns.size == (num - 1 + extra)
      }

      val cc = Interval.closed(x, y) // [x, y]
      val oo = Interval.open(x, y) // (x, y)
      val oc = Interval.openLower(x, y) // (x, y]
      val co = Interval.openUpper(x, y) // [x, y)

      val step = (y - x) / num

      if (step.isZero) {
        List(cc, oo, oc, co).forall { xs =>
          Try(xs.iterator(0)).isFailure
        }
      } else {
        val triples = List((cc, true, true), (oo, false, false), (oc, false, true), (co, true, false))
        triples.forall { case (interval, hasLower, hasUpper) =>
          testEndpoints(interval, step, hasLower, hasUpper) &&
            testEndpoints(interval, -step, hasLower, hasUpper)
        }
      }
    }
  }

  property("half-unbound intervals are ok") {
    forAll { (n: Rational, s: Rational) =>

      val step0 = s.abs

      val cu = Interval.atOrAbove(n) // [n, ∞)
      val ou = Interval.above(n) // (n, ∞)
      val uc = Interval.atOrBelow(n) // (-∞, n]
      val uo = Interval.below(n) // (-∞, n)

      if (step0.isZero) {
        List(cu, ou, uc, uo).forall { xs =>
          Try(xs.iterator(0)).isFailure
        }
      } else {
        val triples = List((cu, true, 1), (ou, false, 1), (uc, true, -1), (uo, false, -1))
        triples.forall { case (interval, hasN, mult) =>
          val step = step0 * mult
          val it = interval.iterator(step)
          val expected = if (hasN) n else n + step
          it.next() == expected &&
          Try(interval.iterator(-step)).isFailure
        }
      }
    }
  }

  property("unbound intervals are not supported") {
    forAll { (step: Rational) =>
      Try(Interval.all[Rational].iterator(step)).isFailure
    }
  }
}
