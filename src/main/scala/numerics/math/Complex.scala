package numerics.math

import scala.math.{ScalaNumber, ScalaNumericConversions}
import scala.math.{Pi, atan2, cos, exp, log, pow, sin, sqrt}

import Implicits._

// TODO: refactor places where Fractional is converted to Double in order to
// access functions (e.g. trig, pow, exp, log)

object Complex {
  def i[T:Fractional] = Complex(fractional.zero, fractional.one)
  def one[T:Fractional] = Complex(fractional.one, fractional.zero)
  def zero[T:Fractional] = Complex(fractional.zero, fractional.zero)

  def fractionalToComplex[T:Fractional](t:T) = new Complex(t, fractional.zero)

  //def lexicographicOrdering[T:Fractional] = Ordering.by((c:Complex[T]) => c.asTuple)
  //
  //def realOrdering[T:Fractional] = Ordering.by((_:Complex[T]).real)
  //def imagOrdering[T:Fractional] = Ordering.by((_:Complex[T]).imag)
  //def magnitudeOrdering[T:Fractional] = Ordering.by((_:Complex[T]).magnitude)

  def polar[T:Fractional](magnitude:T, angle:T) = {
    val real:T = magnitude * fractional.fromDouble(cos(angle.toDouble))
    val imag:T = magnitude * fractional.fromDouble(sin(angle.toDouble))
    Complex[T](real, imag)
  }

  def apply[T:Fractional](real:T, imag:T) = new Complex(real, imag)
}

class Complex[T](val real:T, val imag:T)(implicit f:Fractional[T])
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

  override def equals(that: Any): Boolean = that match {
    case that:Complex[_] => real == that.real && imag == that.imag
    case that => unifiedPrimitiveEquals(that)
  }

  override def toString: String = "Complex(%s, %s)".format(real, imag)

  // ugh, for very large Fractional values this will totally break
  lazy val magnitude: T = f.fromDouble(sqrt((real * real + imag * imag).toDouble))
  lazy val angle: T = f.fromDouble(atan2(imag.toDouble, real.toDouble))
  def abs: T = magnitude
  def arg: T = angle

  def conjugate = Complex(real, -imag)

  def asTuple: (T, T) = (real, imag)
  def asPolarTuple: (T, T) = (abs, arg)

  def isImaginary: Boolean = real == f.zero && imag != f.zero
  def isReal: Boolean = real != f.zero && imag == f.zero

  def eq(b:Complex[T]) = real == b.real && imag == b.imag

  def unary_-() = Complex(-real, -imag)

  def +(b:Complex[T]) = Complex(real + b.real, imag + b.imag)

  def -(b:Complex[T]) = Complex(real - b.real, imag - b.imag)

  def *(b:Complex[T]) = Complex(real * b.real - imag * b.imag,
                                  imag * b.real + real * b.imag)

  def /(b:Complex[T]) = {
    val abs_breal = b.real.abs
    val abs_bimag = b.imag.abs

    if (abs_breal >= abs_bimag) {
      if (abs_breal === f.zero) throw new Exception("/ by zero")
      val ratio = b.imag / b.real
      val denom = b.real + b.imag * ratio
      Complex((real + imag * ratio) / denom, (imag - real * ratio) / denom)

    } else {
      if (abs_bimag === f.zero) throw new Exception("/ by zero")
      val ratio = b.real / b.imag
      val denom = b.real * ratio + b.imag
      Complex((real * ratio + imag) / denom, (imag * ratio - real) / denom)
    }
  }

  def pow(b:Complex[T]) = if (b.eq(Complex.zero[T])) {
    Complex.one[T]

  } else if (this.eq(Complex.zero[T])) {
    if ((b.imag !== f.zero) || (b.real < f.zero))
      throw new Exception("raising 0 to negative/complex power")
    Complex.zero[T]

  } else if (b.imag !== f.zero) {
    val len = f.fromDouble(math.pow(abs.toDouble, b.real.toDouble) / exp((angle * b.imag).toDouble))
    val phase = f.fromDouble(angle.toDouble * b.real.toDouble + log(abs.toDouble) * b.imag.toDouble)
    Complex.polar(len, phase)

  } else {
    val len = f.fromDouble(math.pow(abs.toDouble, b.real.toDouble))
    val phase = angle * b.real
    Complex.polar(len, phase)
  }
}
