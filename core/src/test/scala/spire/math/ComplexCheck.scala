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
    (x - y).abs should be <= threshold

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
