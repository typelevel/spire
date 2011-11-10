package numerics.math

import scala.math.{Pi, atan2, cos, exp, log, pow, sin, sqrt}

import Implicits._

object GComplex {
  def i[T:Fractional] = GComplex(fractional.zero, fractional.one)
  def one[T:Fractional] = GComplex(fractional.one, fractional.zero)
  def zero[T:Fractional] = GComplex(fractional.zero, fractional.zero)

  implicit def fractionalToGComplex[T:Fractional](t:T) = GComplex(t, fractional.zero)

  def lexicographicOrdering[T:Fractional] = Ordering.by((c:GComplex[T]) => c.asTuple)

  def realOrdering[T:Fractional] = Ordering.by((_:GComplex[T]).real)
  def imagOrdering[T:Fractional] = Ordering.by((_:GComplex[T]).imag)
  def magnitudeOrdering[T:Fractional] = Ordering.by((_:GComplex[T]).magnitude)

  def polar[T:Fractional](magnitude:T, angle:T) = {
    val real = magnitude * fractional.fromDouble(cos(angle.toDouble))
    val imag = magnitude * fractional.fromDouble(sin(angle.toDouble))
    GComplex[T](real, imag)
  }
}

case class GComplex[T:Fractional](real:T, imag:T) {
  lazy val magnitude = fractional.fromDouble(sqrt((real * real + imag * imag).toDouble))
  lazy val angle = fractional.fromDouble(atan2(imag.toDouble, real.toDouble))

  def abs = magnitude

  def arg = angle

  def conjugate = GComplex(real, -imag)

  def asTuple = (real, imag)

  def asPolarTuple = (abs, arg)

  def eq(b:GComplex[T]) = real == b.real && imag == b.imag

  def +(b:GComplex[T]) = GComplex(real + b.real, imag + b.imag)

  def -(b:GComplex[T]) = GComplex(real - b.real, imag - b.imag)

  def *(b:GComplex[T]) = GComplex(real * b.real - imag * b.imag,
                                  imag * b.real + real * b.imag)

  def /(b:GComplex[T]) = {
    val abs_breal = b.real.abs
    val abs_bimag = b.imag.abs

    if (abs_breal >= abs_bimag) {
      if (abs_breal === fractional.zero) throw new Exception("/ by zero")
      val ratio = b.imag / b.real
      val denom = b.real + b.imag * ratio
      GComplex((real + imag * ratio) / denom, (imag - real * ratio) / denom)
    } else {
      if (abs_bimag === fractional.zero) throw new Exception("/ by zero")
      val ratio = b.real / b.imag
      val denom = b.real * ratio + b.imag
      GComplex((real * ratio + imag) / denom, (imag * ratio - real) / denom)
    }
  }

  def pow(b:GComplex[T]) = if (b.eq(GComplex.zero[T])) {
    GComplex.one[T]
  } else if (this.eq(GComplex.zero[T])) {
    if ((b.imag !== fractional.zero) || (b.real < fractional.zero))
      throw new Exception("raising 0 to negative/complex power")
    GComplex.zero[T]
  } else if (b.imag !== fractional.zero) {
    val len = fractional.fromDouble(math.pow(abs.toDouble, b.real.toDouble) / exp((angle * b.imag).toDouble))
    val phase = fractional.fromDouble(angle.toDouble * b.real.toDouble + log(abs.toDouble) * b.imag.toDouble)
    GComplex.polar(len, phase)
  } else {
    val len = fractional.fromDouble(math.pow(abs.toDouble, b.real.toDouble))
    val phase = angle * b.real
    GComplex.polar(len, phase)
  }
}
