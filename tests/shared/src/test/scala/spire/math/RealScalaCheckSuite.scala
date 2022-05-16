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
import spire.laws.arb.{rational, real}

import ArbitrarySupport._
import Ordinal._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

class RealScalaCheckSuite extends munit.ScalaCheckSuite {

  val pi200 =
    "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"

  val e200 =
    "2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357290033429526059563073813232862794349076323382988075319525101901"

  val sqrtTwo200 =
    "1.41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157273501384623091229702492483605585073721264412149709993583141322266592750559275579995050115278206057147"

  property("pi") { Real.pi.getString(200) == pi200 }
  property("e") { Real.e.getString(200) == e200 }
  property("sqrt(2)") { Real(2).sqrt.getString(200) == sqrtTwo200 }

  property("Rational(n) = Real(n).toRational") {
    forAll { (n: BigInt) =>
      Rational(n) == Real(n).toRational
    }
  }

  property("Real(n)/Real(d) = Real(n/d)") {
    forAll { (r: Rational) =>
      Real(r.numerator) / Real(r.denominator) == Real(r)
    }
  }

  property("x + 0 = x") {
    forAll { (x: Real) =>
      x + Real.zero == x
    }
  }

  property("x * 0 = 0") {
    forAll { (x: Real) =>
      x * Real.zero == Real.zero
    }
  }

  property("x * 1 = x") {
    forAll { (x: Real) =>
      x + Real.zero == x
    }
  }

  property("x + y = y + x") {
    forAll { (x: Real, y: Real) =>
      x + y == y + x
    }
  }

  property("x + (-x) = 0") {
    forAll { (x: Real) =>
      x + (-x) == Real.zero
    }
  }

  property("x / x = 1") {
    forAll { (x: Real) =>
      (x != 0) ==> { x / x == Real.one }
    }
  }

  property("x * y = y * x") {
    forAll { (x: Real, y: Real) =>
      x * y == y * x
    }
  }

  property("x + x = 2x") {
    forAll { (x: Real) =>
      x + x == x * Real(2)
    }
  }

  property("x * (y + z) = xy + xz") {
    forAll { (x: Real, y: Real, z: Real) =>
      x * (y + z) == x * y + x * z
    }
  }

  property("x.pow(2) = x * x") {
    forAll { (x: Real) =>
      x.pow(2) == x * x
    }
  }

  property("x.pow(3) = x * x * x") {
    forAll { (x: Real) =>
      x.pow(3) == x * x * x
    }
  }

  property("pythagorean theorem") {
    forAll { (y: Real, x: Real) =>
      (x.signum != 0 || y.signum != 0) ==> {
        val mag = (x.pow(2) + y.pow(2)).sqrt
        val x0 = x / mag
        val y0 = y / mag
        x0.pow(2) + y0.pow(2) == Real(1)
      }
    }
  }

  // since atan2 has branch cuts, we limit the magnitue of x and y
  property("sin(atan2(y, x)) = y/mag, cos(atan2(y, x)) = x/mag") {
    forAll { (yn: Long, yd: Long, xn: Long, xd: Long) =>
      (xd != 0 && yd != 0 && (xn != 0 || yn != 0)) ==> {
        val x = Real(Rational(xn, xd))
        val y = Real(Rational(yn, yd))
        val mag = (x ** 2 + y ** 2).sqrt
        Real.sin(Real.atan2(y, x)) == (y / mag) &&
        Real.cos(Real.atan2(y, x)) == (x / mag)
      }
    }
  }

  property("x.round = (((x * 2).floor + 1) / 2).floor") {
    forAll { (x0: Rational) =>
      val x = Real(x0)
      if (x.signum >= 0) {
        x.round == (((x * 2).floor + 1) / 2).floor
      } else {
        x.round == (((x * 2).ceil - 1) / 2).ceil
      }
    }
  }

  property("x.floor <= x.round <= x.ceil") {
    forAll { (x: Real) =>
      x.floor <= x.round &&
      x.round <= x.ceil
    }
  }

  property("complex multiplication".ignore) {
    // too slow to use irrational numbers to test here
    forAll { (re0: Rational, im0: Rational) =>
      val re = Real(re0)
      val im = Real(im0)

      val ma = (re.pow(2) + im.pow(2)).sqrt
      val ph = Real.atan2(im, re)

      val ma2 = ma.pow(2)
      val ph2 = ph * Real(2)

      ma2 * Real.cos(ph2) == re.pow(2) - im.pow(2)
      ma2 * Real.sin(ph2) == re * im * Real(2)
    }
  }

  def arcSample(f: Rational => Rational)(g: Double => Double, h: Real => Real): String =
    (-8 to 8).map { i =>
      val x = Real(f(Rational(i)))
      if ((g(x.toDouble) - h(x).toDouble).abs < 0.00001) "." else "!"
    }.mkString

  // useful for visually debugging atan/asin
  test("atan sample") {
    arcSample(_ / 2)(scala.math.atan, Real.atan)
  }

  test("asin sample") {
    arcSample(_ / 8)(scala.math.asin, Real.asin)
  }

  test("acos sample") {
    arcSample(_ / 8)(scala.math.acos, Real.acos)
  }

  // TODO: this doesn't really work due to the kind of rounding that
  // even computable reals introduce when computing 1/3.
  property("x.pow(j).nroot(k) = x.fpow(j/k)") {
    forAll { (x0: Int, j0: Byte, k0: Byte) =>
      (x0 > 0) ==> {
        val x = Real(x0)
        val j = (j0 & 0xff) % 10 + 1
        val k = (k0 & 0xff) % 10 + 1
        x.pow(j).nroot(k) == x.fpow(Rational(j, k))
      }
    }
  }

  property("x.pow(k) = x.fpow(k)") {
    forAll { (x: Real, k: Byte) =>
      x.pow(k & 0xff) == x.fpow(Rational(k & 0xff))
    }
  }
}
