package spire.math

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

class QuaternionCheck extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  // to fudge some of the properties, we limit our quaternion terms to
  // integers in [-1000, 1000]. this is cheating, and we should do
  // better in the future.
  def df(n: Int): Double = (n % 1000).toDouble

  implicit val arbq = Arbitrary(for {
    r <- arbitrary[Int]
    i <- arbitrary[Int]
    j <- arbitrary[Int]
    k <- arbitrary[Int]
  } yield {
    Quaternion(df(r), df(i), df(j), df(k))
  })

  property("q + 0 = q") {
    forAll { (q: Quaternion) =>
      q + 0.0 should be === q
      q + Quaternion.zero should be === q
    }
  }

  property("q + -q = 0") {
    forAll { (q: Quaternion) =>
      q + (-q) should be === Quaternion.zero
    }
  }

  property("q1 + -q2 = q1 - q2") {
    forAll { (q1: Quaternion, q2: Quaternion) =>
      q1 + (-q2) should be === q1 - q2
    }
  }

  property("q1 + q2 = q2 + q1") {
    forAll { (q1: Quaternion, q2: Quaternion) =>
      q1 + q2 should be === q2 + q1
    }
  }

  property("(q1 + q2) + a3 = q1 + (q2 + q3)") {
    forAll { (q1: Quaternion, q2: Quaternion, q3: Quaternion) =>
      (q1 + q2) + q3 should be === q1 + (q2 + q3)
    }
  }

  property("q * 0 = q") {
    forAll { (q: Quaternion) =>
      q * 0.0 should be === Quaternion.zero
      q * Quaternion.zero should be === Quaternion.zero
    }
  }

  property("q * 1 = q") {
    forAll { (q: Quaternion) =>
      q * 1.0 should be === q
      q * Quaternion.one should be === q
    }
  }

  property("q * 2 = q + q") {
    forAll { (q: Quaternion) =>
      q * 2.0 should be === q + q
    }
  }

  property("q1 * (q2 + q3) = q1 * q2 + q1 * q3") {
    forAll { (q1: Quaternion, q2: Quaternion, q3: Quaternion) =>
      q1 * (q2 + q3) should be === q1 * q2 + q1 * q3
    }
  }

  property("(q1 * q2) * a3 = q1 * (q2 * q3)") {
    forAll { (q1: Quaternion, q2: Quaternion, q3: Quaternion) =>
      (q1 * q2) * q3 should be === q1 * (q2 * q3)
    }
  }

  property("q * q.reciprocal = 1") {
    forAll { (q: Quaternion) =>
      (q * q.reciprocal - Quaternion.one).norm should be < 1e-6
    }
  }

  property("1 / q = 1.reciprocal") {
    forAll { (q: Quaternion) =>
      if (q != Quaternion.zero)
        (Quaternion.one / q - q.reciprocal).norm should be < 1e-6
    }
  }

  property("q ** 2 = q * q") {
    forAll { (q: Quaternion) =>
      q ** 2 should be === q * q
    }
  }

  property("q.sqrt ** 2 = q") {
    forAll { (q: Quaternion) =>
      (q.sqrt ** 2 - q).norm should be < 1e-6
    }
  }


  property("q.sqrt = q.nroot(2)") {
    forAll { (q: Quaternion) =>
      (q.sqrt - q.nroot(2)).norm should be < 1e-6
    }
  }

  property("q.nroot(k) ** k = q") {
    forAll { (q: Quaternion, k0: Int) =>
      val k = (k0 % 10).abs + 1
      (q - q.nroot(k).pow(k)).norm should be < 1e-6
    }
  }

  property("q.fpow(1/k) = q.nroot(k)") {
    forAll { (q: Quaternion, k0: Int) =>
      val k = (k0 % 10).abs + 1
      (q.nroot(k) - q.fpow(1.0/k)).norm should be < 1e-6
    }
  }

  property("q.fpow(1/k).fpow(k) = q") {
    forAll { (q: Quaternion, k0: Double) =>
      val k = (k0 % 10).abs
      if (k == 0.0) {
        q.fpow(k) should be === Quaternion.one
      } else {
        (q - q.fpow(1.0/k).fpow(k)).norm should be < 1e-6
      }
    }
  }

  property("q = q.r iff q.isReal") {
    forAll { (q: Quaternion) =>
      q == q.r should be === q.isReal
    }
  }

  property("q.hashCode = c.hashCode") {
    import spire.std.double._

    forAll { (r: Double, i: Double) =>
      val q = Quaternion(r, i, 0.0, 0.0)
      val c = Complex(r, i)
      q.hashCode should be === c.hashCode
    }
  }

  property("q = c") {
    import spire.std.double._

    forAll { (r: Double, i: Double) =>
      Quaternion(r, i, 0.0, 0.0) should be === Complex(r, i)
    }

    forAll { (r: Double, i: Double, j: Double, k: Double) =>
      if (j != 0.0 || k != 0.0) {
        Quaternion(r, i, j, k) != Complex(r, i) should be === true
      }
    }
  }
}
