package spire.math

import spire.implicits._

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import ArbitrarySupport._

class QuaternionCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  type H = Quaternion[CReal]
  val zero = Quaternion.zero[CReal]
  val one = Quaternion.one[CReal]

  property("q + 0 = q") {
    forAll { (q: H) =>
      q + CReal.zero shouldBe q
      q + zero shouldBe q
    }
  }

  property("q + -q = 0") {
    forAll { (q: H) =>
      q + (-q) shouldBe zero
    }
  }

  property("q1 + -q2 = q1 - q2") {
    forAll { (q1: H, q2: H) =>
      q1 + (-q2) shouldBe q1 - q2
    }
  }

  property("q1 + q2 = q2 + q1") {
    forAll { (q1: H, q2: H) =>
      q1 + q2 shouldBe q2 + q1
    }
  }

  property("(q1 + q2) + a3 = q1 + (q2 + q3)") {
    forAll { (q1: H, q2: H, q3: H) =>
      (q1 + q2) + q3 shouldBe q1 + (q2 + q3)
    }
  }

  property("q * 0 = q") {
    forAll { (q: H) =>
      q * CReal.zero shouldBe zero
      q * zero shouldBe zero
    }
  }

  property("q * 1 = q") {
    forAll { (q: H) =>
      q * CReal.one shouldBe q
      q * one shouldBe q
    }
  }

  property("q * 2 = q + q") {
    forAll { (q: H) =>
      q * CReal(2) shouldBe q + q
    }
  }

  property("q1 * (q2 + q3) = q1 * q2 + q1 * q3") {
    forAll { (q1: H, q2: H, q3: H) =>
      q1 * (q2 + q3) shouldBe q1 * q2 + q1 * q3
    }
  }

  property("(q1 * q2) * a3 = q1 * (q2 * q3)") {
    forAll { (q1: H, q2: H, q3: H) =>
      (q1 * q2) * q3 shouldBe q1 * (q2 * q3)
    }
  }

  property("q * q.reciprocal = 1") {
    forAll { (q: H) =>
      if (q != zero) (q * q.reciprocal) shouldBe one
    }
  }

  property("1 / q = 1.reciprocal") {
    forAll { (q: H) =>
      if (q != zero) (one / q) shouldBe q.reciprocal
    }
  }

  property("q.pow(2) = q * q") {
    forAll { (q: H) =>
      q.pow(2) shouldBe q * q
    }
  }

  property("q.sqrt.pow(2) = q") {
    forAll { (q: H) =>
      q.sqrt.pow(2) shouldBe q
    }
  }

  // // doesn't quite work yet
  // property("q.nroot(3).pow(3) = q") {
  //   //forAll { (q: H) =>
  //   forAll { (a: Short, b: Short, c: Short, d: Short) =>
  //     val qr = Quaternion(CReal(a), CReal(b), CReal(c), CReal(d))
  //     qr.nroot(3).pow(3) shouldBe qr
  //   }
  // }

  // property("q.nroot(k).pow(k) = q") {
  //   forAll { (q: H, k0: Int) =>
  //     val k = (k0 % 10).abs + 1
  //     q.nroot(k).pow(k) shouldBe q
  //   }
  // }
  // 
  // property("q.fpow(1/k) = q.nroot(k)") {
  //   forAll { (q: H, k0: Int) =>
  //     val k = (k0 % 10).abs + 1
  //     q.nroot(k) shouldBe q.fpow(CReal(Rational(1, k)))
  //   }
  // }
  // 
  // property("q.fpow(1/k).fpow(k) = q") {
  //   forAll { (q: H, k0: Byte) =>
  //     val k = CReal(Rational((k0 % 10).abs))
  //     val ik = k.reciprocal
  //     if (k == CReal.zero) {
  //       q.fpow(k) shouldBe one
  //     } else {
  //       q.fpow(ik).fpow(k) shouldBe q
  //     }
  //   }
  // }

  property("q = q.r iff q.isReal") {
    forAll { (q: H) =>
      q == q.r shouldBe q.isReal
    }
  }

  // property("q.hashCode = c.hashCode") {
  //   forAll { (r: CReal, i: CReal) =>
  //     val q = Quaternion(r, i, CReal.zero, CReal.zero)
  //     val c = Complex(r, i)
  //     q.hashCode shouldBe c.hashCode
  //   }
  // }

  property("q = c") {
    forAll { (r: CReal, i: CReal) =>
      Quaternion(r, i, CReal.zero, CReal.zero) shouldBe Complex(r, i)
    }

    forAll { (r: CReal, i: CReal, j: CReal, k: CReal) =>
      Quaternion(r, i, j, k) == Complex(r, i) shouldBe (j == CReal.zero && k == CReal.zero)
    }
  }
}
