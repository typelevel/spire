package spire
package math

import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import spire.laws.arb._

class TrileanCheck extends PropSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  property("associativity") {
    forAll { (x: Trilean, y: Trilean, z: Trilean) =>
      ((x & y) & z) shouldBe (x & (y & z))
      ((x | y) | z) shouldBe (x | (y | z))
    }
  }

  property("commutativity") {
    forAll { (x: Trilean, y: Trilean) =>
      (x & y) shouldBe (y & x)
      (x | y) shouldBe (y | x)
    }
  }

  property("absorption") {
    forAll { (x: Trilean, y: Trilean) =>
      (x & (x | y)) shouldBe x
      (x | (x & y)) shouldBe x
    }
  }

  property("identity") {
    forAll { (x: Trilean) =>
      (x & Trilean.True) shouldBe x
      (x | Trilean.False) shouldBe x
    }
  }

  property("distributivity") {
    forAll { (x: Trilean, y: Trilean, z: Trilean) =>
      (x & (y | z)) shouldBe ((x & y) | (x & z))
      (x | (y & z)) shouldBe ((x | y) & (x | z))
    }
  }

  property("Boolean equivalence") {
    forAll { (x: Boolean, y: Boolean) =>
      val tx = Trilean(x)
      val ty = Trilean(y)
      !tx shouldBe Trilean(!x)
      !ty shouldBe Trilean(!y)
      tx & ty shouldBe Trilean(x & y)
      tx | ty shouldBe Trilean(x | y)
      tx ^ ty shouldBe Trilean(x ^ y)
    }
  }
}
