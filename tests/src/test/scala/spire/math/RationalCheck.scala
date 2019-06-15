package spire
package math

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class RationalCheck extends PropSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  type Q = Rational

  implicit val arbRational: Arbitrary[Rational] = Arbitrary(for {
    n <- arbitrary[BigInt]
    d0 <- arbitrary[BigInt]
  } yield {
    val d = if (d0.signum == 0) BigInt(1) else d0
    Rational(n, d)
  })

  def rat1(name: String)(f: Q => Unit) =
    property(name) {
      forAll { (nx: Long, _dx: Long) =>
        val dx = if (_dx == 0) 1 else _dx
        f(Rational(nx, dx))
      }
    }

  def rat2(name: String)(f: (Q, Q) => Unit) =
    property(name) {
      forAll { (nx: Long, _dx: Long, ny: Long, _dy: Long) =>
        val dx = if (_dx == 0) 1 else _dx
        val dy = if (_dy == 0) 1 else _dy
        f(Rational(nx, dx), Rational(ny, dy))
      }
    }

  def rat3(name: String)(f: (Q, Q, Q) => Unit) =
    property(name) {
      forAll { (nx: Long, _dx: Long, ny: Long, _dy: Long, nz: Long, _dz: Long) =>
        val dx = if (_dx == 0) 1 else _dx
        val dy = if (_dy == 0) 1 else _dy
        val dz = if (_dz == 0) 1 else _dz
        f(Rational(nx, dx), Rational(ny, dy), Rational(nz, dz))
      }
    }

  property("Internal GCD implementation is similar to the field of fractions implementation") {
    forAll { (x: Rational, y: Rational) => x.gcd(y) shouldBe Rational(x.numerator gcd y.numerator, x.denominator lcm y.denominator) }
  }

  rat1("x + 0 == x") { x: Q => x + Rational(0) shouldBe x }
  rat1("x * 1 == x") { x: Q => x * Rational(1) shouldBe x }
  rat1("x * 0 == 0") { x: Q => x * Rational(0) shouldBe Rational(0) }

  rat1("x.floor <= x.round <= x.ceil") { x: Q =>
    x.floor should be <= x.round
    x.round should be <= x.ceil
  }

  rat1("x + x == 2x") { x: Q => (x + x) shouldBe 2 * x }
  rat1("x - x == 0") { x: Q => x - x shouldBe Rational(0) }
  rat1("x * x == x^2") { x: Q => (x * x) shouldBe x.pow(2) }
  rat1("(x^-1)^3 == x^-3") { x: Q => if (x != 0) x.reciprocal.pow(3) shouldBe x.pow(-3) }
  rat1("x / x == 1") { x: Q => if (x != 0) x / x shouldBe Rational(1) }

  rat2("x + y == y + x") { (x: Q, y: Q) => x + y shouldBe y + x }
  rat2("x - y == -y + x") { (x: Q, y: Q) => x - y shouldBe -y + x }
  rat2("x + y - x == y") { (x: Q, y: Q) => (x + y) - x shouldBe y }
  rat2("x / y == x * (y^-1)") { (x: Q, y: Q) => if (y != 0) x / y shouldBe x * y.reciprocal }

  rat3("(x + y) * z == x * z + y * z") { (x: Q, y: Q, z: Q) => (x + y) * z shouldBe x * z + y * z }

  rat1("Round-trip to Real") { (x: Q) =>
    x.toReal.toRational shouldBe x
  }

  rat1("Round-trip to Algebraic") { (x: Q) =>
    x.toAlgebraic.toRational shouldBe Some(x)
  }

  property("Round-trip Double") {
    forAll("x") { (n: Double) =>
      Rational(n).toDouble == n
    }
  }

  property("limitToInt does not change small Rationals") {
    forAll { (n: Int, d: Int) =>
      val r = Rational(n, if (d < 1) 1 else d)
      r.limitToInt shouldBe r
    }
  }

  property("limitToInt regression") {
    val n = Int.MinValue
    val r = Rational(n, 1)
    r.limitToInt shouldBe r
  }

  property("Rational.numeratorIsValidLong") { (x: Q) =>
    x.numeratorIsValidLong shouldBe x.numerator.isValidLong
  }

  property("Rational.denominatorIsValidLong") { (x: Q) =>
    x.denominatorIsValidLong shouldBe x.denominator.isValidLong
  }

  property("limitTo(n) forces numerator and denominator to be less than n") {
    implicit val arbSafeLong: Arbitrary[SafeLong] =
      Arbitrary(arbitrary[BigInt].map { n => SafeLong(n.abs) }.filter(_.signum != 0))

    forAll { (x: Rational, n: SafeLong) =>
      val y = x.limitTo(n.abs)
      (y.numerator <= n) shouldBe true
      (y.denominator <= n) shouldBe true
    }
  }
}
