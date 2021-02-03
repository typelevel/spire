package spire
package math

import spire.laws.arb.uint
import org.scalacheck.Prop._

class UIntSuite extends munit.ScalaCheckSuite {

  val zero = UInt(0)
  val one = UInt(1)

  property("n >= 0") {
    forAll { (n: UInt) =>
      n >= zero == true
    }
  }

  property("a + b == b + a") {
    forAll { (a: UInt, b: UInt) => a + b == b + a }
  }

  property("a * b == b * a") {
    forAll { (a: Int, b: Int) => a * b == b * a }
  }

  property("(a + b) - b == a") {
    forAll { (a: UInt, b: UInt) => (a + b) - b == a }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: UInt) =>
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
    forAll { (n: UInt) =>
      n / one == n
      n % one == zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: UInt, d: UInt) =>
      (d != zero) ==> {
        val q = n / d
        val r = n % d
        q * d + r == n
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: UInt, d: UInt) =>
      (d != zero) ==> { n / d <= n == true }
    }
  }

  property("n % d < d") {
    forAll { (n: UInt, d: UInt) =>
      (d != zero) ==> { n % d < d == true }
    }
  }

  property("n + 1 > n") {
    forAll { (n: UInt) =>
      (n != UInt.MaxValue) ==> { n + one > n == true }
    }
  }

  property("n + (-n) == 0") {
    forAll { (n: UInt) => n + (-n) == zero }
  }

  property("a < b") {
    forAll { (a: UInt, b: UInt) => a < b == a.toLong < b.toLong }
  }

  property("a <= b") {
    forAll { (a: UInt, b: UInt) => a <= b == a.toLong <= b.toLong }
  }

  property("a > b") {
    forAll { (a: UInt, b: UInt) => a > b == a.toLong > b.toLong }
  }

  property("a >= b") {
    forAll { (a: UInt, b: UInt) => a >= b == a.toLong >= b.toLong }
  }
}
