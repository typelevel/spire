package spire.math

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

class RationalCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  type Q = Rational

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
}
