package numerics.math

import org.scalatest.FunSuite

class GComplexTest extends FunSuite {
  test("create GComplex[Double]") {
    val (real, imag) = (3.0, 44.0)
    val c = GComplex(real, imag)
    assert(c.real == real)
    assert(c.imag == imag)
  }

  test("create GComplex[BigDecimal]") {
    val (real, imag) = (BigDecimal(222.0), BigDecimal(3483.0))
    val c = GComplex(real, imag)
    assert(c.real == real)
    assert(c.imag == imag)
  }
}
