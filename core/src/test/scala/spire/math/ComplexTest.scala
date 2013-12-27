package spire.math

import org.scalatest.FunSuite
import spire.implicits.{eqOps => _, _}
import java.math.MathContext

class ComplexTest extends FunSuite {
  test("create Complex[Double]") {
    val (real, imag) = (3.0, 44.0)
    val c = Complex(real, imag)
    assert(c.real === real)
    assert(c.imag === imag)
    assert(c === c)
  }

  test("create Complex[BigDecimal]") {

    implicit val mc = MathContext.DECIMAL128

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

  test("complex arithmetic") {
    val i = Complex.i[Double]

    val a = 4.0 + 3.0*i
    val b = 1.0 + 2.0*i
    val c = 2.0 + 0.0*i

    assert(a + b === 5.0+5.0*i)
    assert(b + c === Complex(3.0, 2.0))
    assert(b + c === Complex(3.0, 2.0))    

    assert(a - b === Complex(3.0, 1.0))
    assert(b - c === Complex(-1.0, 2.0))
    assert(a - c === Complex(2.0, 3.0))

    assert(a * b === Complex(-2.0, 11.0))
    assert(b * c === Complex(2.0, 4.0))
    assert(a * c === Complex(8.0, 6.0))

    assert(a / b === Complex(2.0, -1.0))
    assert(b / c === Complex(0.5, 1.0))
    assert(a / c === Complex(2.0, 1.5))
  }

  test("test e^(i * pi) with Double") {
    val e = Complex(scala.math.E, 0.0)
    val pi = Complex(scala.math.Pi, 0.0)
    val i = Complex.i[Double]
    val one = Complex.one[Double]

    val z = e.pow(i * pi) + one
    assert (z.real === 0.0)
    assert (z.imag < 0.000000000000001) // sigh...
    assert (z.imag > -0.000000000000001)
  }

  test("test roots of unity") {
    val one = Complex.one[Double]
    val i = Complex.i[Double]

    assert(Complex.rootsOfUnity[Double](2) === Array(one, -one))
    assert(Complex.rootsOfUnity[Double](4) === Array(one, i, -one, -i))

    val theta = 2.0 * math.Pi / 3.0
    val c1 = math.cos(theta) + math.sin(theta) * i
    val c2 = -one - c1
    assert(Complex.rootsOfUnity[Double](3) === Array(one, c1, c2))
  }

  test("try using FastComplex") {
    val fc = FastComplex

    val a = fc(3.0, -2.0)
    val b = fc(2.0, 1.0)

    assert(fc.add(a, b) === fc(5.0, -1.0))
    assert(fc.subtract(a, b) === fc(1.0, -3.0))
    assert(fc.multiply(a, b) === fc(8.0, -1.0))

    val e = fc(scala.math.E, 0.0)
    val pi = fc(scala.math.Pi, 0.0)

    val ipi = fc.multiply(fc.i, pi)
    val e_ipi = fc.pow(e, ipi)
    val z = fc.add(e_ipi, fc.one)

    println((fc.real(z), fc.imag(z)))
    assert(fc.real(z) == 0.0F)
    assert(fc.imag(z) < 0.000000001F)
  }

  test("try using FloatComplex") {
    val fc = FastComplex

    val a = FloatComplex(3.0, -2.0)
    val b = FloatComplex(2.0, 1.0)

    assert(a + b === FloatComplex(5.0, -1.0))
    assert(a - b === FloatComplex(1.0, -3.0))
    assert(a * b === FloatComplex(8.0, -1.0))

    val i = FloatComplex.i
    val one = FloatComplex.one
    val e = FloatComplex(scala.math.E, 0.0)
    val pi = FloatComplex(scala.math.Pi, 0.0)

    val z = e.pow(i * pi) + one
    
    assert(z.real == 0.0F)
    assert(z.imag < 0.000000001F)
  }
}
