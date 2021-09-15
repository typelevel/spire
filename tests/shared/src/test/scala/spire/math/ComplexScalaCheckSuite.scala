package spire
package math

import spire.std.any._
import org.scalacheck.Prop._

class ComplexScalaCheckSuite extends munit.ScalaCheckSuite {
  type C = Complex[BigDecimal]

  scala.util.Random.setSeed(123)

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

  implicit val threshold: BigDecimal = BigDecimal(1e-20)

  def near(x: Complex[BigDecimal], y: Complex[BigDecimal])(implicit threshold: BigDecimal) =
    if (x == y) x == y else (x - y).abs <= threshold

  def logNear(x: Complex[BigDecimal], y: Complex[BigDecimal])(implicit threshold: BigDecimal) =
    if (x == y)
      x == y
    else
      log(x / y).abs <= threshold

  complex1("x + 0 == x") { (x: C) => x + zero == x }
  complex1("x * 1 == x") { (x: C) => x * one == x }
  complex1("x * 0 == 0") { (x: C) => x * zero == zero }
  complex1("x - x == 0") { (x: C) => x - x == zero }
  complex1("x / x == 1") { (x: C) => if (x != zero) near(x / x, one) }
  complex1("x + x == 2x") { (x: C) => near(x + x, x * 2) }

  complex2("x + y == y + x") { (x: C, y: C) => near(x + y, y + x) }
  complex2("x + y - x == y") { (x: C, y: C) => near(x + y - x, y) }
  complex2("(x / y) * y == x") { (x: C, y: C) => if (y != zero) near((x / y) * y, x) }

  complex1("x.sqrt.pow(2) = x") { (x: C) =>
    implicit val threshold: BigDecimal = BigDecimal(2e-9) // 28254913+1i gives a log-error-ratio of 2.02e-9
    logNear(x.sqrt.pow(2), x)
  }

  // use x*x instead of x.pow(2) because of rounding issues with the latter resulting in some brittleness about whether
  // a subsequent sqrt ends up in the first or fourth quadrants
  complex1("(x*x).sqrt = x") { (x: C) =>
    implicit val threshold: BigDecimal = BigDecimal(3e-9) // 1+110201870i has log-error-ratio 2.4e-9
    // Complex.sqrt returns the root with non-negative real value (and +i in the case of -1); adjust the "expected" RHS
    // accordingly
    if (x.real.signum < 0 || (x.real.signum == 0 && x.imag.signum < 0))
      logNear((x * x).sqrt, -x)
    else
      logNear((x * x).sqrt, x)
  }

}
