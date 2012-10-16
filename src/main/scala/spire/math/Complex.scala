package spire.math

import spire.algebra._

import scala.{specialized => spec}
import scala.math.{ScalaNumber, ScalaNumericConversions}
import scala.math.{Pi, atan2, cos, exp, log, sin, sqrt}

import spire.math.fun._
import spire.implicits._

import language.implicitConversions

object Complex {
  def i[@spec(Float, Double) T](implicit f:Fractional[T], t:Trig[T]) =
    new Complex(f.zero, f.one)

  def one[@spec(Float, Double) T](implicit f:Fractional[T], t:Trig[T]) =
    new Complex(f.one, f.zero)

  def zero[@spec(Float, Double) T](implicit f:Fractional[T], t:Trig[T]) =
    new Complex(f.zero, f.zero)

  def fromInt[@spec(Float, Double) T](n: Int)(implicit f:Fractional[T], t:Trig[T]) =
    new Complex(f.fromInt(n), f.zero)

  implicit def intToComplex(n:Int) = new Complex(n.toDouble, 0.0)
  implicit def longToComplex(n:Long) = new Complex(n.toDouble, 0.0)
  implicit def floatToComplex(n:Float) = new Complex(n, 0.0F)
  implicit def doubleToComplex(n:Double) = new Complex(n, 0.0)

  implicit def bigIntToComplex(n:BigInt) = new Complex(BigDecimal(n), BigDecimal(0))
  implicit def bigDecimalToComplex(n:BigDecimal) = new Complex(n, BigDecimal(0))

  def polar[@spec(Float, Double) T](magnitude:T, angle:T)(implicit f:Fractional[T], t:Trig[T]) = {
    val real:T = f.times(magnitude, t.cos(angle))
    val imag:T = f.times(magnitude, t.sin(angle))
    new Complex(real, imag)
  }

  def apply[@spec(Float, Double) T:Fractional:Trig](real:T, imag:T) = new Complex(real, imag)
  def unapply[@spec(Float, Double) T:Fractional:Trig](c:Complex[T]) = Some((c.real, c.imag))
}

final class Complex[@spec(Float, Double) T]
(val real:T, val imag:T)(implicit f:Fractional[T], t:Trig[T])
extends ScalaNumber with ScalaNumericConversions with Serializable {

  // ugh, ScalaNumericConversions ghetto
  //
  // maybe complex numbers are too different...
  def doubleValue = f.toDouble(real)
  def floatValue = f.toFloat(real)
  def longValue = f.toLong(real)
  def intValue = f.toInt(real)
  override def shortValue = f.toShort(real)
  override def byteValue = f.toByte(real)
  def isWhole = f.isWhole(real) && f.eqv(imag, f.zero)
  def signum: Int = f.compare(real, f.zero)
  def underlying = (real, imag)
  def complexSignum = if (f.eqv(abs, f.zero)) Complex.zero else this / new Complex(abs, f.zero)

  override def hashCode: Int = {
    if (f.isWhole(real) && f.eqv(imag, f.zero) &&
        f.lteqv(real, f.fromInt(Int.MaxValue)) &&
        f.gteqv(real, f.fromInt(Int.MinValue))) f.toInt(real)
    else 19 * real.## + 41 * imag.##
  }

  // not typesafe, so this is the best we can do :(
  override def equals(that: Any): Boolean = that match {
    case that:Complex[_] => real == that.real && imag == that.imag
    case that => unifiedPrimitiveEquals(that)
  }

  override def toString: String = "Complex(%s, %s)".format(real, imag)

  // ugh, specialized lazy vals don't work very well
  //lazy val abs: T = f.sqrt(real * real + imag * imag)
  //lazy val arg: T = t.atan2(imag, real)
  def abs: T = f.sqrt(f.plus(f.times(real, real), f.times(imag, imag)))
  def arg: T = t.atan2(imag, real)

  def conjugate = new Complex(real, f.negate(imag))

  def asTuple: (T, T) = (real, imag)
  def asPolarTuple: (T, T) = (abs, arg)

  def isImaginary: Boolean = f.eqv(real, f.zero) && f.neqv(imag, f.zero)
  def isReal: Boolean = f.neqv(real, f.zero) && f.eqv(imag, f.zero)

  def eqv(b:Complex[T]) = f.eqv(real, b.real) && f.eqv(imag, b.imag)
  def neqv(b:Complex[T]) = f.neqv(real, b.real) || f.neqv(imag, b.imag)

  def unary_-() = new Complex(f.negate(real), f.negate(imag))

  def +(b:Complex[T]) = new Complex(
    f.plus(real, b.real),
    f.plus(imag, b.imag)
  )

  def -(b:Complex[T]) = new Complex(
    f.minus(real, b.real),
    f.minus(imag, b.imag)
  )

  def *(b:Complex[T]) = new Complex(
    f.minus(f.times(real, b.real), f.times(imag, b.imag)),
    f.plus(f.times(imag, b.real), f.times(real, b.imag))
  )

  def /(b:Complex[T]) = {
    val abs_breal = f.abs(b.real)
    val abs_bimag = f.abs(b.imag)

    if (f.gteqv(abs_breal, abs_bimag)) {
      if (f.eqv(abs_breal, f.zero)) throw new Exception("/ by zero")
      val ratio = f.div(b.imag, b.real)
      val denom = f.plus(b.real, f.times(b.imag, ratio))
      new Complex(
        f.div(f.plus(real, f.times(imag, ratio)), denom),
        f.div(f.minus(imag, f.times(real, ratio)), denom)
      )

    } else {
      if (f.eqv(abs_bimag, f.zero)) throw new Exception("/ by zero")
      val ratio = f.div(b.real, b.imag)
      val denom = f.plus(f.times(b.real, ratio), b.imag)
      new Complex(
        f.div(f.plus(f.times(real, ratio), imag), denom),
        f.div(f.minus(f.times(imag, ratio), real), denom)
      )
    }
  }

  // TODO: to avoid creating intermediate objects we should probably implement
  // all of these directly rather than relying on our previous definitions.
  //
  // alternately, could try piecewise functions for the previous things
  // (e.g. div_real(Complex, Complex) which would return just real part).
  def quot(b:Complex[T]) = new Complex(f.floor((this / b).real), f.zero)

  def /~(b:Complex[T]) = quot(b)

  def %(b:Complex[T]):Complex[T] = this - (b * (this /~ b))

  def /%(b:Complex[T]):(Complex[T], Complex[T]) = {
    val q = quot(b)
    (q, this - (b * q))
  }

  def **(b:Int): Complex[T] = pow(b)

  def pow(b:Int): Complex[T] = pow(Complex.fromInt[T](b))

  def **(b:Complex[T]): Complex[T] = pow(b)

  def pow(b:Complex[T]): Complex[T] = if (b.eqv(Complex.zero[T])) {
    Complex.one[T]
  } else if (this.eqv(Complex.zero[T])) {
    if (f.neqv(b.imag, f.zero) || f.lt(b.real, f.zero))
      throw new Exception("raising 0 to negative/complex power")
    Complex.zero[T]
  } else if (f.neqv(b.imag, f.zero)) {
    val a = f.fpow(abs, b.real)
    val bb = f.times(arg, b.imag)
    val c = t.exp(bb)
    val len = f.div(a, c)
    val d = f.times(arg, b.real)
    val e = f.log(abs)
    val ff = f.times(e, b.imag)
    val phase = f.plus(d, ff)
    Complex.polar(len, phase)
  } else {
    Complex.polar(f.fpow(abs, b.real), f.times(arg, b.real))
  }

  // we are going with the "principal value" definition of Log.
  def log:Complex[T] = {
    if (this == Complex.zero[T]) sys.error("log undefined at 0")
    Complex(f.log(abs), arg)
  }
}


// TODO: if/when scala gets value types this gets even more interesting
/**
 * FastComplex is an ugly, beautiful hack.
 *
 * The basic idea is to encode two 32-bit Floats into a single 64-bit Long.
 * The lower-32 bits are the "real" Float and the upper-32 are the "imaginary"
 * Float.
 *
 * Since we're overloading the meaning of Long, all the operations have to be
 * defined on the FastComplex object, meaning the syntax for using this is a
 * bit ugly. To add to the ugly beauty of the whole thing I could imagine
 * defining implicit operators on Long like +@, -@, *@, /@, etc.
 *
 * You might wonder why it's even worth doing this. The answer is that when
 * you need to allocate an array of e.g. 10-20 million complex numbers, the GC
 * overhead of using *any* object is HUGE. Since we can't build our own
 * "pass-by-value" types on the JVM we are stuck doing an encoding like this.
 *
 * Here are some profiling numbers for summing an array of complex numbers,
 * timed against a concrete case class implementation using Float (in ms):
 *
 *  size | encoded |  class
 *    1M |     5.1 |    5.8
 *    5M |    28.5 |   91.7
 *   10M |    67.7 |  828.1
 *   20M |   228.0 | 2687.0
 *
 * Not bad, eh?
 */
object FastComplex {
  // note the superstitious use of @inline and final everywhere

  final def apply(real:Float, imag:Float) = encode(real, imag)
  final def apply(real:Double, imag:Double) = encode(real.toFloat, imag.toFloat)

  // encode a float as some bits
  @inline final def bits(n:Float):Int = java.lang.Float.floatToRawIntBits(n)

  // decode some bits into a float
  @inline final def bits(n:Int):Float = java.lang.Float.intBitsToFloat(n)

  // get the real part of the complex number
  @inline final def real(d:Long):Float = bits((d & 0xffffffff).toInt)

  // get the imaginary part of the complex number
  @inline final def imag(d:Long):Float = bits((d >> 32).toInt)

  // define some handy constants
  final val i = encode(0.0F, 1.0F)
  final val one = encode(1.0F, 0.0F)
  final val zero = encode(0.0F, 0.0F)

  // encode two floats representing a complex number
  @inline final def encode(real:Float, imag:Float):Long = {
    (bits(imag).toLong << 32) + bits(real).toLong
  }

  // encode two floats representing a complex number in polar form
  @inline final def polar(magnitude:Float, angle:Float):Long = {
    encode(magnitude * cos(angle).toFloat, magnitude * sin(angle).toFloat)
  }

  // decode should be avoided in fast code because it allocates a Tuple2.
  final def decode(d:Long):(Float, Float) = (real(d), imag(d))

  // produces a string representation of the Long/(Float,Float)
  final def toRepr(d:Long):String = "FastComplex(%s -> %s)" format(d, decode(d))

  // get the magnitude/absolute value
  final def abs(d:Long):Float = {
    val re = real(d)
    val im = imag(d)
    sqrt(re * re + im * im).toFloat
  }

  // get the angle/argument
  final def angle(d:Long):Float = atan2(imag(d), real(d)).toFloat

  // get the complex conjugate
  final def conjugate(d:Long):Long = encode(real(d), -imag(d))

  // see if the complex number is a whole value
  final def isWhole(d:Long):Boolean = real(d) % 1.0F == 0.0F && imag(d) % 1.0F == 0.0F

  // get the sign of the complex number
  final def signum(d:Long):Int = real(d) compare 0.0F

  // get the complex sign of the complex number
  final def complexSignum(d:Long):Long = {
    val m = abs(d)
    if (m == 0.0F) zero else divide(d, encode(m, 0.0F))
  }

  // negation
  final def negate(a:Long):Long = encode(-real(a), -imag(a))

  // addition
  final def add(a:Long, b:Long):Long = encode(real(a) + real(b), imag(a) + imag(b))

  // subtraction
  final def subtract(a:Long, b:Long):Long = encode(real(a) - real(b), imag(a) - imag(b))

  // multiplication
  final def multiply(a:Long, b:Long):Long = {
    val re_a = real(a)
    val im_a = imag(a)
    val re_b = real(b)
    val im_b = imag(b)
    encode(re_a * re_b - im_a * im_b, im_a * re_b + re_a * im_b)
  }

  // division
  final def divide(a:Long, b:Long):Long = {
    val re_a = real(a)
    val im_a = imag(a)
    val re_b = real(b)
    val im_b = imag(b)

    val abs_re_b = math.abs(re_b)
    val abs_im_b = math.abs(im_b)

    if (abs_re_b >= abs_im_b) {
      if (abs_re_b == 0.0F) sys.error("/0")
      val ratio = im_b / re_b
      val denom = re_b + im_b * ratio
      encode((re_a + im_a * ratio) / denom, (im_a - re_a * ratio) / denom)

    } else {
      if (abs_im_b == 0.0F) sys.error("/0")
      val ratio = re_b / im_b
      val denom = re_b * ratio + im_b
      encode((re_a * ratio + im_a) / denom, (im_a * ratio - re_a) / denom)
    }
  }

  def quot(a:Long, b:Long):Long = {
    encode(scala.math.floor(real(divide(a, b))).toFloat, 0.0F)
  }

  def mod(a:Long, b:Long):Long = subtract(a, multiply(b, quot(a, b)))

  def quotmod(a:Long, b:Long):(Long, Long) = {
    val q = quot(a, b)
    (q, subtract(a, multiply(b, quot(a, b))))
  }

  // exponentiation
  final def pow(a:Long, b:Long):Long = if (b == zero) {
    encode(1.0F, 0.0F)

  } else if (a == zero) {
    if (imag(b) != 0.0F || real(b) < 0.0F)
      throw new Exception("raising 0 to negative/complex power")
    zero

  } else if (imag(b) != 0.0F) {
    val im_b = imag(b)
    val re_b = real(b)
    val len = (math.pow(abs(a), re_b) / exp((angle(a) * im_b))).toFloat
    val phase = (angle(a) * re_b + log(abs(a)) * im_b).toFloat
    polar(len, phase)

  } else {
    val len = math.pow(abs(a), real(b)).toFloat
    val phase = (angle(a) * real(b)).toFloat
    polar(len, phase)
  }
}

object FloatComplex {
  import FastComplex.{encode}

  final def apply(real:Float, imag:Float): FloatComplex =
    new FloatComplex(encode(real, imag))

  final def apply(real:Double, imag:Double) =
    new FloatComplex(encode(real.toFloat, imag.toFloat))

  def polar(magnitude: Float, angle: Float) =
    new FloatComplex(FastComplex.polar(magnitude, angle))

  final val i = new FloatComplex(4575657221408423936L)
  final val one = new FloatComplex(1065353216L)
  final val zero = new FloatComplex(0L)
}

class FloatComplex(val u:Long) extends AnyVal {
  def real: Float = FastComplex.real(u)
  def imag: Float = FastComplex.imag(u)
  def repr = "FloatComplex(%s, %s)" format(real, imag)
  def abs: Float = FastComplex.abs(u)
  def angle: Float = FastComplex.angle(u)
  def conjugate = new FloatComplex(FastComplex.conjugate(u))
  def isWhole: Boolean = FastComplex.isWhole(u)
  def signum: Int = FastComplex.signum(u)
  def complexSignum = new FloatComplex(FastComplex.complexSignum(u))
  def negate = new FloatComplex(FastComplex.negate(u))

  def +(b:FloatComplex) = new FloatComplex(FastComplex.add(u, b.u))
  def -(b:FloatComplex) = new FloatComplex(FastComplex.subtract(u, b.u))
  def *(b:FloatComplex) = new FloatComplex(FastComplex.multiply(u, b.u))
  def /(b:FloatComplex) = new FloatComplex(FastComplex.divide(u, b.u))
  def /~(b:FloatComplex) = new FloatComplex(FastComplex.quot(u, b.u))
  def %(b:FloatComplex) = new FloatComplex(FastComplex.mod(u, b.u))

  def /%(b:FloatComplex) = FastComplex.quotmod(u, b.u) match {
    case (q, m) => (new FloatComplex(q), new FloatComplex(m))
  }

  def pow(b:FloatComplex) = new FloatComplex(FastComplex.pow(u, b.u))
  def **(b:FloatComplex) = pow(b)

  def pow(b:Int) = new FloatComplex(FastComplex.pow(u, FastComplex(b.toFloat, 0.0F)))
  def **(b:Int) = pow(b)
}
