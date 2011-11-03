package numerics.math

import scala.math.{Pi, atan2, cos, exp, log, pow, sin, sqrt}

object Complex {
  val i = Complex(0, 1)
  val one = Complex(1, 0)
  val zero = Complex(0, 0)

  implicit def doubleToComplex(d:Double) = Complex(d, 0)

  val lexicographicOrdering = Ordering.by((c:Complex) => c.asTuple)

  val realOrdering = Ordering.by((_:Complex).real)
  val imagOrdering = Ordering.by((_:Complex).imag)
  val magnitudeOrdering = Ordering.by((_:Complex).magnitude)

  def polar(magnitude:Double, angle:Double) = {
    val real = magnitude * cos(angle)
    val imag = magnitude * sin(angle)
    Complex(real, imag)
  }
}

case class Complex(real:Double, imag:Double) {
  lazy val magnitude = sqrt(real * real + imag * imag)
  lazy val angle = atan2(imag, real)

  def abs() = magnitude

  def arg() = angle

  def conjugate() = Complex(real, -imag)

  def asTuple() = (real, imag)

  def asPolarTuple() = (abs(), arg())

  def equiv(b:Complex) = real == b.real && imag == b.imag

  def +(b:Complex) = Complex(real + b.real, imag + b.imag)

  def -(b:Complex) = Complex(real - b.real, imag - b.imag)

  // I've commented out the polar forms

  //def *(b:Complex) = Complex.polar(magnitude * b.magnitude, angle + b.angle)
  def *(b:Complex) = Complex(real * b.real - imag * b.imag,
                             imag * b.real + real * b.imag)

  //def /(b:Complex) = if (b.equiv(Complex.zero)) {
  //  throw new Exception("/ by zero")
  //} else {
  //  Complex.polar(magnitude / b.magnitude, angle - b.angle)
  //}
  def /(b:Complex) = {
    val abs_breal = math.abs(b.real)
    val abs_bimag = math.abs(b.imag)

    if (abs_breal >= abs_bimag) {
      if (abs_breal == 0.0) throw new Exception("/ by zero")
      val ratio = b.imag / b.real
      val denom = b.real + b.imag * ratio
      Complex((real + imag * ratio) / denom, (imag - real * ratio) / denom)
    } else {
      if (abs_bimag == 0.0) throw new Exception("/ by zero")
      val ratio = b.real / b.imag
      val denom = b.real * ratio + b.imag
      Complex((real * ratio + imag) / denom, (imag * ratio - real) / denom)
    }
  }

  def pow(b:Complex) = if (b.equiv(Complex.zero)) {
    Complex.one
  } else if (this.equiv(Complex.zero)) {
    if (b.imag != 0 || b.real < 0)
      throw new Exception("raising 0 to negative/complex power")
    Complex.zero
  } else if (b.imag != 0.0) {
    val len = math.pow(abs, b.real) / exp(angle * b.imag)
    val phase = angle * b.real + log(abs) * b.imag
    Complex.polar(len, phase)
  } else {
    val len = math.pow(abs, b.real)
    val phase = angle * b.real
    Complex.polar(len, phase)
  }
}
