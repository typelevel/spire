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

  // exact checking isn't quite working in all cases, ugh
  val tolerance = CReal(Rational(1, 1000000000))

  import spire.compat.ordering

  def dumpDiff(label: String, base: H, gen: H) {
    println(s"$label $base $gen")
    val (gr, gi, gj, gk) = (gen.r, gen.i, gen.j, gen.k)
    val (br, bi, bj, bk) = (base.r, base.i, base.j, base.k)
    if (br != gr) println(s"  r: ${br.repr} != ${gr.repr} (${br.toRational} and ${gr.toRational}) [${(br-gr).signum}] <${br-gr}>")
    if (bi != gi) println(s"  i: ${bi.repr} != ${gi.repr} (${bi.toRational} and ${gi.toRational}) [${(bi-gi).signum}] <${bi-gi}>")
    if (bj != gj) println(s"  j: ${bj.repr} != ${gj.repr} (${bj.toRational} and ${gj.toRational}) [${(bj-gj).signum}] <${bj-gj}>")
    if (bk != gk) println(s"  k: ${bk.repr} != ${gk.repr} (${bk.toRational} and ${gk.toRational}) [${(bk-gk).signum}] <${bk-gk}>")
  }

  def inexactEq(x: H, y: H): Unit =
    if (x != y) {
      //dumpDiff("ouch", x, y)
      (x - y).abs should be < tolerance // sadface
    } else {
      x shouldBe y
    }

  property("q.sqrt.pow(2) = q") {
    forAll { (q: H) =>
      val r = q.sqrt.pow(2)
      inexactEq(q, r)
    }
  }

  property("q.nroot(3).pow(3) = q") {
    forAll { (a: Short, b: Short, c: Short, d: Short) =>
      val q = Quaternion(CReal(a), CReal(b), CReal(c), CReal(d))
      val r = q.nroot(3).pow(3)
      inexactEq(q, r)
    }
  }

  property("q.nroot(k).pow(k) = q") {
    forAll { (a: Short, b: Short, c: Short, d: Short, k0: Int) =>
      val q = Quaternion(CReal(a), CReal(b), CReal(c), CReal(d))
      val k = (k0 % 5).abs + 1
      val r = q.nroot(k).pow(k)
      inexactEq(q, r)
    }
  }

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

  property("q.hashCode = c.hashCode") {
    forAll { (r: CReal, i: CReal) =>
      val q1 = Quaternion(r, i, CReal.zero, CReal.zero)
      val c1 = Complex(r, i)
      q1.hashCode shouldBe c1.hashCode

      val q2 = Quaternion(r)
      val c2 = Complex(r)
      q2.hashCode shouldBe c2.hashCode
      q2.hashCode shouldBe r.hashCode
    }
  }

  property("q = c") {
    val z = CReal.zero
    forAll { (r: CReal, i: CReal) =>
      Quaternion(r, i, z, z) shouldBe Complex(r, i)
      Quaternion(r, z, z, z) shouldBe Complex(r, z)
      Quaternion(z, i, z, z) shouldBe Complex(z, i)
    }

    forAll { (r: CReal, i: CReal, j: CReal, k: CReal) =>
      Quaternion(r, i, j, k) == Complex(r, i) shouldBe (j == CReal.zero && k == CReal.zero)
    }
  }
}
