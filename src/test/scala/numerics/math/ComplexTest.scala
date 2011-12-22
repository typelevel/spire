package numerics.math

import org.scalatest.FunSuite

class ComplexTest extends FunSuite {
  test("create Complex[Double]") {
    val (real, imag) = (3.0, 44.0)
    val c = Complex(real, imag)
    assert(c.real === real)
    assert(c.imag === imag)
    assert(c === c)
  }

  test("create Complex[BigDecimal]") {
    val (real, imag) = (BigDecimal(222.0), BigDecimal(3483.0))
    val c = Complex(real, imag)
    assert(c.real === real)
    assert(c.imag === imag)
    assert(c === c)
  }

  test("some basic equality stuff") {
    val one = Complex.one[Double]
    val i = Complex.i[Double]

    assert(one === 1)
    assert(one === 1.0)
    assert(one === Complex.one[Double])

    assert(1 === one)
    assert(1.0 === one)
    assert(Complex.one[Double] === one)

    assert(1 != i)
    assert(1.0 != i)
    assert(one != i)

    assert(i != 1)
    assert(i != 1.0)
    assert(i != one)
  }

  test("test e^(i * pi) with Double") {
    import scala.math._
    val e = Complex(E, 0.0)
    val pi = Complex(Pi, 0.0)
    val i = Complex.i[Double]
    val one = Complex.one[Double]

    val z = e.pow(i * pi) + one
    assert (z.real === 0.0)
    assert (z.imag < 0.000000000000001) // sigh...
  }

  // TODO: once Complex stops using conversions to Double to do exp/pow/log,
  // we should try out Euler's identity with Rational.

  // brings up the point that it would be nice to have functions for e, pi, etc
  // for Rational/BigDecimal/etc that gave us as much precision as we asked for.
  //
  // or, symbolic Real types anyone? why not reimplement mathematica?
}
