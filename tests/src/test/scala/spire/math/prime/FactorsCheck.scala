package spire
package math.prime

import spire.implicits._
import spire.laws.arb.safeLong
import spire.math.SafeLong

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import spire.math.ArbitrarySupport._
import Ordinal._

class FactorsCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

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
/* Compile error with 2.13
  property("Factors(n).pow(k) = n.pow(k)") {
    forAll { (n: Long, k: Sized[Int, _1, _10]) =>
      Factors(n).pow(k.num).value shouldBe SafeLong(n).pow(k.num)
    }
  }
*/
}
