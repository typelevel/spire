package spire.math

import org.scalatest.FunSuite
import spire.math.fun._
import Implicits.{eqOps => _, _}
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
  }

  // TODO: get a way to get Real values for e and pi, and try this for Real

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
    
    assert(fc.real(z) == 0.0F)
    assert(fc.imag(z) < 0.000000001F)
  }
}
