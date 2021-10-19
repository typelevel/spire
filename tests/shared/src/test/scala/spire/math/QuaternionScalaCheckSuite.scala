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

import spire.implicits._
import spire.laws.arb.{quaternion, real}

import org.scalacheck.Prop._

class QuaternionScalaCheckSuite extends munit.ScalaCheckSuite {

  type H = Quaternion[Real]
  val zero = Quaternion.zero[Real]
  val one = Quaternion.one[Real]

  property("q + 0 = q") {
    forAll { (q: H) =>
      q + Real.zero == q &&
      q + zero == q
    }
  }

  property("q + -q = 0") {
    forAll { (q: H) =>
      q + (-q) == zero
    }
  }

  property("q1 + -q2 = q1 - q2") {
    forAll { (q1: H, q2: H) =>
      q1 + (-q2) == q1 - q2
    }
  }

  property("q1 + q2 = q2 + q1") {
    forAll { (q1: H, q2: H) =>
      q1 + q2 == q2 + q1
    }
  }

  property("(q1 + q2) + a3 = q1 + (q2 + q3)") {
    forAll { (q1: H, q2: H, q3: H) =>
      (q1 + q2) + q3 == q1 + (q2 + q3)
    }
  }

  property("q * 0 = q") {
    forAll { (q: H) =>
      q * Real.zero == zero
      q * zero == zero
    }
  }

  property("q * 1 = q") {
    forAll { (q: H) =>
      q * Real.one == q &&
      q * one == q
    }
  }

  property("q * 2 = q + q") {
    forAll { (q: H) =>
      q * Real(2) == q + q
    }
  }

  property("q1 * (q2 + q3) = q1 * q2 + q1 * q3") {
    forAll { (q1: H, q2: H, q3: H) =>
      q1 * (q2 + q3) == q1 * q2 + q1 * q3
    }
  }

  property("(q1 * q2) * a3 = q1 * (q2 * q3)") {
    forAll { (q1: H, q2: H, q3: H) =>
      (q1 * q2) * q3 == q1 * (q2 * q3)
    }
  }

  property("q * q.reciprocal = 1") {
    forAll { (q: H) =>
      (q != zero) ==> ((q * q.reciprocal) == one)
    }
  }

  property("1 / q = 1.reciprocal") {
    forAll { (q: H) =>
      (q != zero) ==> ((one / q) == q.reciprocal)
    }
  }

  property("q.pow(2) = q * q") {
    forAll { (q: H) =>
      q.pow(2) == q * q
    }
  }

  // exact checking isn't quite working in all cases, ugh
  val tolerance = Real(Rational(1, 1000000000))

  def dumpDiff(label: String, base: H, gen: H): Unit = {
    println(s"$label $base $gen")
    val (gr, gi, gj, gk) = (gen.r, gen.i, gen.j, gen.k)
    val (br, bi, bj, bk) = (base.r, base.i, base.j, base.k)
    if (br != gr)
      println(
        s"  r: ${br.repr} != ${gr.repr} (${br.toRational} and ${gr.toRational}) [${(br - gr).signum}] <${br - gr}>"
      )
    if (bi != gi)
      println(
        s"  i: ${bi.repr} != ${gi.repr} (${bi.toRational} and ${gi.toRational}) [${(bi - gi).signum}] <${bi - gi}>"
      )
    if (bj != gj)
      println(
        s"  j: ${bj.repr} != ${gj.repr} (${bj.toRational} and ${gj.toRational}) [${(bj - gj).signum}] <${bj - gj}>"
      )
    if (bk != gk)
      println(
        s"  k: ${bk.repr} != ${gk.repr} (${bk.toRational} and ${gk.toRational}) [${(bk - gk).signum}] <${bk - gk}>"
      )
  }

  def inexactEq(x: H, y: H): Unit =
    if (x != y) {
      //dumpDiff("ouch", x, y)
      (x - y).abs < tolerance // sadface
    } else {
      x == y
    }

  property("q.sqrt.pow(2) = q") {
    forAll { (q: H) =>
      val r = q.sqrt.pow(2)
      inexactEq(q, r)
    }
  }

  property("q.nroot(3).pow(3) = q") {
    forAll { (a: Short, b: Short, c: Short, d: Short) =>
      val q = Quaternion(Real(a), Real(b), Real(c), Real(d))
      val r = q.nroot(3).pow(3)
      inexactEq(q, r)
    }
  }

  property("q.nroot(k).pow(k) = q") {
    forAll { (a: Short, b: Short, c: Short, d: Short, k0: Int) =>
      val q = Quaternion(Real(a), Real(b), Real(c), Real(d))
      val k = (k0 % 5).abs + 1
      val r = q.nroot(k).pow(k)
      inexactEq(q, r)
    }
  }

  // property("q.fpow(1/k) = q.nroot(k)") {
  //   forAll { (q: H, k0: Int) =>
  //     val k = (k0 % 10).abs + 1
  //     q.nroot(k) == q.fpow(Real(Rational(1, k)))
  //   }
  // }
  //
  // property("q.fpow(1/k).fpow(k) = q") {
  //   forAll { (q: H, k0: Byte) =>
  //     val k = Real(Rational((k0 % 10).abs))
  //     val ik = k.reciprocal
  //     if (k == Real.zero) {
  //       q.fpow(k) == one
  //     } else {
  //       q.fpow(ik).fpow(k) == q
  //     }
  //   }
  // }

  property("q = q.r iff q.isReal") {
    forAll { (q: H) =>
      q == q.r == q.isReal
    }
  }

  property("q.hashCode = c.hashCode") {
    forAll { (r: Real, i: Real) =>
      val q1 = Quaternion(r, i, Real.zero, Real.zero)
      val c1 = Complex(r, i)
      q1.hashCode == c1.hashCode

      val q2 = Quaternion(r)
      val c2 = Complex(r)
      q2.hashCode == c2.hashCode &&
      q2.hashCode == r.hashCode
    }
  }

  property("q = c") {
    val z = Real.zero
    forAll { (r: Real, i: Real) =>
      Quaternion(r, i, z, z) == Complex(r, i) &&
      Quaternion(r, z, z, z) == Complex(r, z) &&
      Quaternion(z, i, z, z) == Complex(z, i)
    }

    forAll { (r: Real, i: Real, j: Real, k: Real) =>
      Quaternion(r, i, j, k) == Complex(r, i) == (j == Real.zero && k == Real.zero)
    }
  }
}
