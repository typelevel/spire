package spire.math

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

class PackageCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  property("fib(n + 2) = fib(n + 1) + fib(n)") {
    forAll { (n0: Byte) =>
      val n = n0.toLong.abs
      fib(n + 2) shouldBe fib(n + 1) + fib(n)
    }
  }

  property("(n + 1)! = n! * (n + 1)") {
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
