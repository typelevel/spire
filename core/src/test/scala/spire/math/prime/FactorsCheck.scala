package spire.math.prime

import spire.math.SafeLong

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import spire.implicits._
import spire.math.ArbitrarySupport._
import Ordinal._

class FactorsCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import Factors.{zero, one}

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
    forAll { (n: Long, m: Long) =>
      whenever (m != 0) {
        (Factors(n) / Factors(m)).value shouldBe SafeLong(n) / SafeLong(m)
      }
    }
  }
  
  property("Factors(n) % Factors(m) = n % m") {
    forAll { (n: Long, m: Long) =>
      whenever (m != 0) {
        (Factors(n) % Factors(m)).value shouldBe SafeLong(n) % SafeLong(m)
      }
    }
  }

  property("Factors(n) /% Factors(m) = n /% m") {
    forAll { (n: Long, m: Long) =>
      whenever (m != 0) {
        val (x, y) = (Factors(n) /% Factors(m))
        (x.value, y.value) shouldBe SafeLong(n) /% SafeLong(m)
      }
    }
  }
  
  property("Factors(n).pow(k) = n.pow(k)") {
    forAll { (n: Long, k: Sized[Int, _1, _10]) =>
      Factors(n).pow(k.num).value shouldBe SafeLong(n).pow(k.num)
    }
  }
}
