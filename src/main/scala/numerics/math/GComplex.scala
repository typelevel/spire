package numerics.math

import scala.math.{ScalaNumber, ScalaNumericConversions}
import scala.math.{Pi, atan2, cos, exp, log, pow, sin, sqrt}

import Implicits._

object GComplex {
  def i[T:Fractional] = GComplex(fractional.zero, fractional.one)
  def one[T:Fractional] = GComplex(fractional.one, fractional.zero)
  def zero[T:Fractional] = GComplex(fractional.zero, fractional.zero)

  def fractionalToGComplex[T:Fractional](t:T) = new GComplex(t, fractional.zero)

  def lexicographicOrdering[T:Fractional] = Ordering.by((c:GComplex[T]) => c.asTuple)

  def realOrdering[T:Fractional] = Ordering.by((_:GComplex[T]).real)
  def imagOrdering[T:Fractional] = Ordering.by((_:GComplex[T]).imag)
  def magnitudeOrdering[T:Fractional] = Ordering.by((_:GComplex[T]).magnitude)

  def polar[T:Fractional](magnitude:T, angle:T) = {
    val real:T = magnitude * fractional.fromDouble(cos(angle.toDouble))
    val imag:T = magnitude * fractional.fromDouble(sin(angle.toDouble))
    GComplex[T](real, imag)
  }

  def apply[T:Fractional](real:T, imag:T) = new GComplex(real, imag)
}

class GComplex[T](val real:T, val imag:T)(implicit f:Fractional[T])
extends ScalaNumber with ScalaNumericConversions with Serializable {

  // ugh, ScalaNumericConversions ghetto
  //
  // maybe complex numbers are too different...
  def doubleValue = real.toDouble
  def floatValue = real.toFloat
  def longValue = real.toLong
  def intValue = real.toInt
  def underlying = List(real, imag)
  def isWhole = real.isWhole && imag.isWhole
  def signum: Int = {
    val i = f.compare(real, f.zero)
    if (i != 0) i else f.compare(imag, f.zero)
  }

  override def hashCode: Int = if (isReal && real.isWhole &&
                                   real <= f.fromInt(Int.MaxValue) &&
                                   real >= f.fromInt(Int.MinValue)) {
    real.toInt.##
  } else {
    19 * real.## + 41 * imag.##
  }

  // -------
  lazy val magnitude = f.fromDouble(sqrt((real * real + imag * imag).toDouble))
  lazy val angle = f.fromDouble(atan2(imag.toDouble, real.toDouble))

  def abs = magnitude

  def arg = angle

  def conjugate = GComplex(real, -imag)

  def asTuple = (real, imag)

  def asPolarTuple = (abs, arg)

  def isImaginary: Boolean = real == f.zero
  def isReal: Boolean = imag == f.zero

  def eq(b:GComplex[T]) = real == b.real && imag == b.imag

  def +(b:GComplex[T]) = GComplex(real + b.real, imag + b.imag)

  def -(b:GComplex[T]) = GComplex(real - b.real, imag - b.imag)

  def *(b:GComplex[T]) = GComplex(real * b.real - imag * b.imag,
                                  imag * b.real + real * b.imag)

  def /(b:GComplex[T]) = {
    val abs_breal = b.real.abs
    val abs_bimag = b.imag.abs

    if (abs_breal >= abs_bimag) {
      if (abs_breal === f.zero) throw new Exception("/ by zero")
      val ratio = b.imag / b.real
      val denom = b.real + b.imag * ratio
      GComplex((real + imag * ratio) / denom, (imag - real * ratio) / denom)
    } else {
      if (abs_bimag === f.zero) throw new Exception("/ by zero")
      val ratio = b.real / b.imag
      val denom = b.real * ratio + b.imag
      GComplex((real * ratio + imag) / denom, (imag * ratio - real) / denom)
    }
  }

  def pow(b:GComplex[T]) = if (b.eq(GComplex.zero[T])) {
    GComplex.one[T]
  } else if (this.eq(GComplex.zero[T])) {
    if ((b.imag !== f.zero) || (b.real < f.zero))
      throw new Exception("raising 0 to negative/complex power")
    GComplex.zero[T]
  } else if (b.imag !== f.zero) {
    val len = f.fromDouble(math.pow(abs.toDouble, b.real.toDouble) / exp((angle * b.imag).toDouble))
    val phase = f.fromDouble(angle.toDouble * b.real.toDouble + log(abs.toDouble) * b.imag.toDouble)
    GComplex.polar(len, phase)
  } else {
    val len = f.fromDouble(math.pow(abs.toDouble, b.real.toDouble))
    val phase = angle * b.real
    GComplex.polar(len, phase)
  }
}
