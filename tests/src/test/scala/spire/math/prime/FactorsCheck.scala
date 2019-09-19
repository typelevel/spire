package spire
package math.prime

import spire.implicits._
import spire.laws.arb.safeLong
import spire.math.SafeLong

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import spire.math.ArbitrarySupport._
import Ordinal._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class FactorsCheck extends AnyPropSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit val arbitraryFactors: Arbitrary[Factors] =
    Arbitrary(arbitrary[SafeLong].map(n => Factors(n)))

  property("Factors(n).value = n") {
    forAll { (n: Long) =>
      Factors(n).value shouldBe n
    }
  }

  property("Factors(n) + Factors(m) = n + m") {
    forAll { (n: Long, m: Long) =>
      (Factors(n) + Factors(m)).value shouldBe SafeLong(n) + SafeLong(m)
    }
  }

  property("Factors(n) - Factors(m) = n - m") {
    forAll { (n: Long, m: Long) =>
      (Factors(n) - Factors(m)).value shouldBe SafeLong(n) - SafeLong(m)
    }
  }

  property("Factors(n) * Factors(m) = n * m") {
    forAll { (n: Long, m: Long) =>
      (Factors(n) * Factors(m)).value shouldBe SafeLong(n) * SafeLong(m)
    }
  }

  property("Factors(n) / Factors(m) = n / m") {
    forAll { (n: Long, nz: NonZero[Long]) =>
      val m = nz.num
      (Factors(n) / Factors(m)).value shouldBe SafeLong(n) / SafeLong(m)
    }
  }

  property("Factors(n) % Factors(m) = n % m") {
    forAll { (n: Long, nz: NonZero[Long]) =>
      val m = nz.num
      (Factors(n) % Factors(m)).value shouldBe SafeLong(n) % SafeLong(m)
    }
  }

  property("Factors(n) /% Factors(m) = n /% m") {
    forAll { (n: Long, nz: NonZero[Long]) =>
      val m = nz.num
      val (x, y) = (Factors(n) /% Factors(m))
      (x.value, y.value) shouldBe SafeLong(n) /% SafeLong(m)
    }
  }

  property("Factors(n).pow(k) = n.pow(k)") {
    forAll { (n: Long, k: Sized[Int, _1, _10]) =>
      Factors(n).pow(k.num).value shouldBe SafeLong(n).pow(k.num)
    }
  }
}
