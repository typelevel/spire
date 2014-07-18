package spire.math

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.implicits._

import BigDecimal.RoundingMode._

class ComplexCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  type C = Complex[BigDecimal]

  val zero = Complex.zero[BigDecimal]
  val one = Complex.one[BigDecimal]

  def complex1(name: String)(f: C => Unit) =
    property(name) {
      forAll { (rx: Int, ix: Int) =>
        f(Complex(BigDecimal(rx), BigDecimal(ix)))
      }
    }

  def complex2(name: String)(f: (C, C) => Unit) =
    property(name) {
      forAll { (rx: Int, ix: Int, ry: Int, iy: Int) =>
        f(Complex(BigDecimal(rx), BigDecimal(ix)), Complex(BigDecimal(ry), BigDecimal(iy)))
      }
    }

  val threshold = BigDecimal(1e-20)
  def near(x: Complex[BigDecimal], y: Complex[BigDecimal]) =
    if (x == y) x shouldBe y else (x - y).abs should be <= threshold

  complex1("x + 0 == x") { x: C => x + zero shouldBe x }
  complex1("x * 1 == x") { x: C => x * one shouldBe x }
  complex1("x * 0 == 0") { x: C => x * zero shouldBe zero }
  complex1("x - x == 0") { x: C => x - x shouldBe zero }
  complex1("x / x == 1") { x: C => if (x != zero) near(x / x, one) }
  complex1("x + x == 2x") { x: C => near(x + x, x * 2) }

  complex2("x + y == y + x") { (x: C, y: C) => near(x + y, y + x) }
  complex2("x + y - x == y") { (x: C, y: C) => near(x + y - x, y) }
  complex2("(x / y) * y == x") { (x: C, y: C) => if (y != zero) near((x / y) * y, x) }
}

class ComplexCheck2 extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  type C = Complex[Real]

  import ArbitrarySupport._

  val zero = Complex.zero[Real]
  val one = Complex.one[Real]

  property("x + 0 = 0 + x = x") {
    forAll { (x: C) =>
      x + zero shouldBe x
      zero + x shouldBe x
    }
  }

  property("x + y = y + x") {
    forAll { (x: C, y: C) =>
      x + y shouldBe y + x
    }
  }

  property("x + (y + z) = (x + y) + z") {
    forAll { (x: C, y: C, z: C) =>
      x + (y + z) shouldBe (x + y) + z
    }
  }

  property("x + (-x) = x - x = 0") {
    forAll { (x: C) =>
      x + (-x) shouldBe zero
      x - x shouldBe zero
    }
  }

  property("x * (y + z) = (x * y) + (x * z)") {
    forAll { (x: C, y: C, z: C) =>
      x * (y + z) shouldBe (x * y) + (x * z)
    }
  }

  property("x * 0 = 0 * x = 0") {
    forAll { (x: C) =>
      x * zero shouldBe zero
      zero * x shouldBe zero
    }
  }

  property("x * 1 = 1 * x = x") {
    forAll { (x: C) =>
      x * one shouldBe x
      one * x shouldBe x
    }
  }

  property("x * (y * z) = (x * y) * z") {
    forAll { (x: C, y: C, z: C) =>
      x * (y * z) shouldBe (x * y) * z
    }
  }

  property("x * y = y * x") {
    forAll { (x: C, y: C) =>
      x * y shouldBe y * x
    }
  }

  property("x / x = 1") {
    forAll { (x: C) =>
      if (x != zero) x / x shouldBe one
    }
  }

  property("x^-1 = 1 / x") {
    forAll { (x: C) =>
      if (x != zero) x.reciprocal shouldBe one / x
    }
  }

  property("x.pow(2) = x * x") {
    forAll { (x: C) =>
      x.pow(2) shouldBe x * x
    }
  }

  import Ordinal._

  // import spire.compat._
  // val threshold = Real("1/1000")
  // def near(x: C, y: C) = (x - y).abs should be <= threshold
  
  // property("x.sqrt.pow(2) = x.pow(2).sqrt = x") {
  //   forAll { (x: C) =>
  //     near(x.sqrt.pow(2), x)
  //     near(x.pow(2).sqrt, x)
  //   }
  // }

  // property("x.nroot(k).pow(k) = x.pow(k).nroot(k) = x") {
  //   forAll { (x: C, k: Sized[Int, _1, _10]) =>
  //     near(x.nroot(k.num).pow(k.num), x)
  //     near(x.pow(k.num).nroot(k.num), x)
  //   }
  // }

  // property("xyz") {
  //   forAll { sz: Sized[Int, _0, _10] =>
  //     sz.num should be >= 0
  //     sz.num should be <= 10
  //   }
  // }
}

class FastComplexCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  property("encode/decode") {
    forAll { (re: Float, im: Float) =>
      val n: Long = FastComplex.encode(re, im)
      val (r, i) = FastComplex.decode(n)

      if (r != re || i != im) {
        val rs = "%x" format FastComplex.bits(re)
        val is = "%x" format FastComplex.bits(im)
        val es = "%x" format n
        println(s"expected $rs $is got $es")
      }

      r shouldBe re
      i shouldBe im
    }
  }
}
