package spire
package math

import spire.implicits._
import spire.laws.arb.{complex, real}
import org.scalacheck.Prop._

class ComplexScalaCheckSuite2 extends munit.ScalaCheckSuite {
  // type C = Complex[Real]
  //
  // val zero = Complex.zero[Real]
  // val one = Complex.one[Real]
  //
  // property("x + 0 = 0 + x = x") {
  //   forAll { (x: C) =>
  //     x + zero == x
  //     zero + x == x
  //   }
  // }
  //
  // property("x + y = y + x") {
  //   forAll { (x: C, y: C) =>
  //     x + y == y + x
  //   }
  // }
  //
  // property("x + (y + z) = (x + y) + z") {
  //   forAll { (x: C, y: C, z: C) =>
  //     x + (y + z) == (x + y) + z
  //   }
  // }
  //
  // property("x + (-x) = x - x = 0") {
  //   forAll { (x: C) =>
  //     x + (-x) == zero
  //     x - x == zero
  //   }
  // }
  //
  // property("x * (y + z) = (x * y) + (x * z)") {
  //   forAll { (x: C, y: C, z: C) =>
  //     x * (y + z) == (x * y) + (x * z)
  //   }
  // }
  //
  // property("x * 0 = 0 * x = 0") {
  //   forAll { (x: C) =>
  //     x * zero == zero
  //     zero * x == zero
  //   }
  // }
  //
  // property("x * 1 = 1 * x = x") {
  //   forAll { (x: C) =>
  //     x * one == x
  //     one * x == x
  //   }
  // }
  //
  // property("x * (y * z) = (x * y) * z") {
  //   forAll { (x: C, y: C, z: C) =>
  //     x * (y * z) == (x * y) * z
  //   }
  // }
  //
  // property("x * y = y * x") {
  //   forAll { (x: C, y: C) =>
  //     x * y == y * x
  //   }
  // }
  //
  // property("x / x = 1") {
  //   forAll { (x: C) =>
  //     (x != zero) ==> (x / x == one)
  //   }
  // }
  //
  // property("x^-1 = 1 / x") {
  //   forAll { (x: C) =>
  //     (x != zero) ==> (x.reciprocal() == one / x)
  //   }
  // }
  //
  // property("x.pow(2) = x * x") {
  //   forAll { (x: C) =>
  //     x.pow(2) == x * x
  //   }
  // }
  //
  // property("c = c.r iff c.isReal") {
  //   forAll { (c: C) =>
  //     c == c.real == c.isReal
  //   }
  // }
  //
  // // import spire.compat._
  // // val threshold = Real("1/1000")
  // // def near(x: C, y: C) = (x - y).abs should be <= threshold
  //
  // // property("x.nroot(k).pow(k) = x.pow(k).nroot(k) = x") {
  // //   forAll { (x: C, k: Sized[Int, _1, _10]) =>
  // //     near(x.nroot(k.num).pow(k.num), x)
  // //     near(x.pow(k.num).nroot(k.num), x)
  // //   }
  // // }
  //
  // // property("xyz") {
  // //   forAll { sz: Sized[Int, _0, _10] =>
  // //     sz.num should be >= 0
  // //     sz.num should be <= 10
  // //   }
  // // }
}
