/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

import spire.implicits._
import java.util.Arrays

class ComplexSuite extends munit.FunSuite {
  test("create Complex[Double]") {
    val (real, imag) = (3.0, 44.0)
    val c = Complex(real, imag)
    assertEquals(c.real, real)
    assertEquals(c.imag, imag)
    assertEquals(c, c)
  }

  test("create Complex[BigDecimal]") {
    val (real, imag) = (BigDecimal(222.0), BigDecimal(3483.0))
    val c = Complex(real, imag)
    assertEquals(c.real, real)
    assertEquals(c.imag, imag)
    assertEquals(c, c)
  }

  test("some basic equality stuff") {
    val one = Complex.one[Double]
    val i = Complex.i[Double]

    assertEquals(one.toInt, 1)
    assertEquals(one.toDouble, 1.0)
    assertEquals(one, Complex.one[Double])

    assertEquals(1, one.toInt)
    assertEquals(1.0: Complex[Double], one)
    assertEquals(Complex.one[Double], one)

    assert(1 != i)
    assert(1.0 != i)
    assert(one != i)

    assert(i != 1)
    assert(i != 1.0)
    assert(i != one)
  }

  test("complex arithmetic") {
    val i = Complex.i[Double]

    val a = 4.0 + 3.0 * i
    val b = 1.0 + 2.0 * i
    val c = 2.0 + 0.0 * i

    assertEquals(a + b, 5.0 + 5.0 * i)
    assertEquals(b + c, Complex(3.0, 2.0))
    assertEquals(b + c, Complex(3.0, 2.0))

    assertEquals(a - b, Complex(3.0, 1.0))
    assertEquals(b - c, Complex(-1.0, 2.0))
    assertEquals(a - c, Complex(2.0, 3.0))

    assertEquals(a * b, Complex(-2.0, 11.0))
    assertEquals(b * c, Complex(2.0, 4.0))
    assertEquals(a * c, Complex(8.0, 6.0))

    assertEquals(a / b, Complex(2.0, -1.0))
    assertEquals(b / c, Complex(0.5, 1.0))
    assertEquals(a / c, Complex(2.0, 1.5))
  }

  test("test e^(i * pi) with Double") {
    val e = Complex(scala.math.E, 0.0)
    val pi = Complex(scala.math.Pi, 0.0)
    val i = Complex.i[Double]
    val one = Complex.one[Double]

    val z = e.pow(i * pi) + one
    assertEquals(z.real, 0.0)
    assert(z.imag < 0.000000000000001) // sigh...
    assert(z.imag > -0.000000000000001)
  }

  test("test roots of unity") {
    val one = Complex.one[Double]
    val i = Complex.i[Double]

    assert(Arrays.equals(Complex.rootsOfUnity[Double](2).toArray[Object], Array[Object](one, -one)))
    assertEquals(Complex.rootOfUnity[Double](2, 0), one)
    assertEquals(Complex.rootOfUnity[Double](2, 1), -one)
    assert(Arrays.equals(Complex.rootsOfUnity[Double](4).toArray[Object], Array[Object](one, i, -one, -i)))
    assertEquals(Complex.rootOfUnity[Double](4, 0), one)
    assertEquals(Complex.rootOfUnity[Double](4, 1), i)
    assertEquals(Complex.rootOfUnity[Double](4, 2), -one)
    assertEquals(Complex.rootOfUnity[Double](4, 3), -i)

    val theta = 2.0 * scala.math.Pi / 3.0
    val c1 = math.cos(theta) + math.sin(theta) * i
    val c2 = -one - c1
    assert(Arrays.equals(Complex.rootsOfUnity[Double](3).toArray[Object], Array[Object](one, c1, c2)))
  }

  test("try using FastComplex") {
    val fc = FastComplex

    val a = fc(3.0, -2.0)
    val b = fc(2.0, 1.0)

    assertEquals(fc.add(a, b), fc(5.0, -1.0))
    assertEquals(fc.subtract(a, b), fc(1.0, -3.0))
    assertEquals(fc.multiply(a, b), fc(8.0, -1.0))

    val e = fc(scala.math.E, 0.0)
    val pi = fc(scala.math.Pi, 0.0)

    val ipi = fc.multiply(fc.i, pi)
    val e_ipi = fc.pow(e, ipi)
    val z = fc.add(e_ipi, fc.one)

    assertEquals(fc.real(z), 0.0f)
    assert(fc.imag(z) < 0.000000001f)

    assertEquals(fc.multiply(fc.i, fc.i), fc(-1f, 0f))
    assertEquals(fc.imag(fc(-1f, 0f)), 0f)
  }

  test("try using FloatComplex") {
    val a = FloatComplex(3.0, -2.0)
    val b = FloatComplex(2.0, 1.0)

    assertEquals(a + b, FloatComplex(5.0, -1.0))
    assertEquals(a - b, FloatComplex(1.0, -3.0))
    assertEquals(a * b, FloatComplex(8.0, -1.0))

    val i = FloatComplex.i
    val one = FloatComplex.one
    val e = FloatComplex(scala.math.E, 0.0)
    val pi = FloatComplex(scala.math.Pi, 0.0)

    val z = e.pow(i * pi) + one

    assertEquals(z.real, 0.0f)
    assert(z.imag < 0.000000001f)
  }

  test("complex trigonometry") {
    // these are just a spot check to avoid sign errors

    assertEquals(Complex(3.0, 5.0).sin, Complex(10.472508533940392, -73.46062169567367))
    assertEquals(Complex(3.0, -5.0).sin, Complex(10.472508533940392, 73.46062169567367))
    assertEquals(Complex(-3.0, 5.0).sin, Complex(-10.472508533940392, -73.46062169567367))
    assertEquals(Complex(-3.0, -5.0).sin, Complex(-10.472508533940392, 73.46062169567367))

    assertEquals(Complex(3.0, 5.0).cos, Complex(-73.46729221264526, -10.471557674805572))
    assertEquals(Complex(3.0, -5.0).cos, Complex(-73.46729221264526, 10.471557674805572))
    assertEquals(Complex(-3.0, 5.0).cos, Complex(-73.46729221264526, 10.471557674805572))
    assertEquals(Complex(-3.0, -5.0).cos, Complex(-73.46729221264526, -10.471557674805572))
  }

  test("complex norm") {
    assertEquals(Complex(3.0, 4.0).norm, 5.0)
    // check against overflow
    assertEquals(Complex(3e20.toFloat, 4e20.toFloat).norm, 5e20.toFloat)
  }
}
