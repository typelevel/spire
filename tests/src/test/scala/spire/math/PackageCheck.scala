package spire
package math

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class PackageCheck extends AnyPropSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  fib(0) shouldBe 0
  fib(1) shouldBe 1
  property("fib(n + 2) = fib(n + 1) + fib(n)") {
    forAll { (n0: Byte) =>
      val n = n0.toLong.abs
      fib(n + 2) shouldBe fib(n + 1) + fib(n)
    }
  }

  property("(n + 1)! = n! * (n + 1)") {
    fact(0) shouldBe 1
    forAll { (n0: Byte) =>
      val n = n0.toLong.abs + 1
      fact(n + 1) shouldBe fact(n) * (n + 1)
    }
  }

  property("choose(n, k) = n!/(k! * (n-k)!)") {
    forAll { (n0: Byte, k0: Byte) =>
      val k = k0.toLong.abs
      val n = n0.toLong.abs
      if (k > n) choose(n, k) shouldBe 0
      else if (k == 0 || k == n) choose(n, k) shouldBe 1
      else choose(n, k) shouldBe fact(n) / (fact(k) * fact(n - k))
    }
  }
}
