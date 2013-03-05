package spire.math

import spire.algebra._
import spire.std.float._
import spire.std.double._
import spire.std.bigDecimal._

import scala.{specialized => spec}
import scala.annotation.tailrec
import scala.math.{ScalaNumber, ScalaNumericConversions}
import java.lang.Math

object Complex extends ComplexInstances {
  def i[@spec(Float, Double) T](implicit f: Fractional[T], t: Trig[T]) =
    new Complex(f.zero, f.one)

  def one[@spec(Float, Double) T](implicit f: Fractional[T], t: Trig[T]) =
    new Complex(f.one, f.zero)

  def zero[@spec(Float, Double) T](implicit f: Fractional[T], t: Trig[T]) =
    new Complex(f.zero, f.zero)

  def fromInt[@spec(Float, Double) T](n: Int)(implicit f: Fractional[T], t: Trig[T]) =
    new Complex(f.fromInt(n), f.zero)

  implicit def intToComplex(n: Int) = new Complex(n.toDouble, 0.0)
  implicit def longToComplex(n: Long) = new Complex(n.toDouble, 0.0)
  implicit def floatToComplex(n: Float) = new Complex(n, 0.0F)
  implicit def doubleToComplex(n: Double) = new Complex(n, 0.0)

  implicit def bigIntToComplex(n: BigInt) =
    bigDecimalToComplex(BigDecimal(n))

  implicit def bigDecimalToComplex(n: BigDecimal) = {
    implicit val mc = n.mc
    new Complex(n, BigDecimal(0))
  }

  def polar[@spec(Float, Double) T](magnitude: T, angle: T)(implicit f: Fractional[T], t: Trig[T]) =
    new Complex(f.times(magnitude, t.cos(angle)), f.times(magnitude, t.sin(angle)))

  def apply[@spec(Float, Double) T](real: T)(implicit f: Fractional[T], t: Trig[T]): Complex[T] =
    new Complex(real, f.zero)
}

final case class Complex[@spec(Float, Double) T](real: T, imag: T)(implicit f: Fractional[T], t: Trig[T])
    extends ScalaNumber with ScalaNumericConversions with Serializable {

  def doubleValue: Double = f.toDouble(real)
  def floatValue: Float = f.toFloat(real)
  def longValue: Long = f.toLong(real)
  def intValue: Int = f.toInt(real)
  override def shortValue: Short = f.toShort(real)
  override def byteValue: Byte = f.toByte(real)

  def isWhole: Boolean = f.eqv(real, f.zero) && f.isWhole(real)
  def signum: Int = f.compare(real, f.zero)
  def underlying: (T, T) = (real, imag)
  def complexSignum: Complex[T] = {
    val a = abs
    if (f.eqv(a, f.zero)) Complex.zero[T] else this / new Complex(a, f.zero)
  }

  override final def isValidInt: Boolean =
    f.eqv(imag, f.zero) &&
    f.isWhole(real) &&
    f.lteqv(real, f.fromInt(Int.MaxValue)) &&
    f.gteqv(real, f.fromInt(Int.MinValue))

  override def hashCode: Int = if (isValidInt)
    f.toInt(real)
  else
    19 * real.## + 41 * imag.## + 97

  // not typesafe, so this is the best we can do :(
  override def equals(that: Any): Boolean = that match {
    case that: Complex[_] => real == that.real && imag == that.imag
    case that => unifiedPrimitiveEquals(that)
  }

  override def toString: String = "(%s + %si)" format (real, imag)

  def abs: T = f.sqrt(f.plus(f.times(real, real), f.times(imag, imag)))
  def arg: T = t.atan2(imag, real)

  def norm: T = f.plus(f.times(real, real), f.times(imag, imag))
  def conjugate = new Complex(real, f.negate(imag))

  def asTuple: (T, T) = (real, imag)
  def asPolarTuple: (T, T) = (abs, arg)

  def isZero: Boolean = f.eqv(real, f.zero) && f.eqv(imag, f.zero)
  def isImaginary: Boolean = f.eqv(real, f.zero)
  def isReal: Boolean = f.eqv(imag, f.zero)

  def eqv(b: Complex[T]) = f.eqv(real, b.real) && f.eqv(imag, b.imag)
  def neqv(b: Complex[T]) = f.neqv(real, b.real) || f.neqv(imag, b.imag)

  def unary_-() = new Complex(f.negate(real), f.negate(imag))

  // (a+ci) + (b+di) = (a+b) + (c+d)i
  def +(b: Complex[T]) =
    new Complex(f.plus(real, b.real), f.plus(imag, b.imag))

  // (a+ci) - (b+di) = (a-b) + (c-d)i
  def -(b: Complex[T]) =
    new Complex(f.minus(real, b.real), f.minus(imag, b.imag))

  // (a+ci) * (b+di) = (a*b - c*d) + (c*b + a*d)i
  def *(b: Complex[T]) = new Complex(
    f.minus(f.times(real, b.real), f.times(imag, b.imag)),
    f.plus(f.times(imag, b.real), f.times(real, b.imag))
  )

  def /(b: Complex[T]) = {
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

  def /~(b: Complex[T]) = {
    val d = this / b
    new Complex(f.round(d.real), f.round(d.imag))
  }

  def %(b: Complex[T]): Complex[T] = this - (this /~ b) * b

  def /%(b: Complex[T]): (Complex[T], Complex[T]) = {
    val q = this /~ b
    (q, this - q * b)
  }

  def **(b: Int): Complex[T] = pow(b)

  def nroot(k: Int): Complex[T] = pow(Complex(f.reciprocal(f.fromInt(k)), f.zero))

  def pow(b: Int): Complex[T] =
    Complex.polar(f.fpow(abs, f.fromInt(b)), f.times(arg, f.fromInt(b)))

  def **(b: Complex[T]): Complex[T] = pow(b)

  def pow(b: Complex[T]): Complex[T] = if (b.eqv(Complex.zero[T])) {
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
    val e = t.log(abs)
    val ff = f.times(e, b.imag)
    val phase = f.plus(d, ff)
    Complex.polar(len, phase)
  } else {
    Complex.polar(f.fpow(abs, b.real), f.times(arg, b.real))
  }

  // we are going with the "principal value" definition of Log.
  def log: Complex[T] = {
    if (this == Complex.zero[T]) sys.error("log undefined at 0")
    new Complex(t.log(abs), arg)
  }

  def sqrt: Complex[T] = {
    val v = f.sqrt(f.div(f.plus(f.abs(real), this.abs), f.fromInt(2)))
    val v2 = f.plus(v, v)
    if (f.gt(real, f.zero))
      new Complex(v, f.div(imag, v2))
    else
      new Complex(f.div(f.abs(imag), v2), f.times(v, f.fromInt(f.signum(imag))))
  }

  def floor: Complex[T] = new Complex(f.floor(real), f.floor(imag))
  def ceil: Complex[T] = new Complex(f.ceil(real), f.ceil(imag))
  def round: Complex[T] = new Complex(f.round(real), f.round(imag))

  // acos(z) = -i*(log(z + i*(sqrt(1 - z*z))))
  def acos: Complex[T] = {
    val z2 = this * this
    val s = new Complex(f.minus(f.one, z2.real), f.negate(z2.imag)).sqrt
    val l = new Complex(f.plus(real, s.imag), f.plus(imag, s.real)).log
    new Complex(l.imag, f.negate(l.real))
  }

  // asin(z) = -i*(log(sqrt(1 - z*z) + i*z))
  def asin: Complex[T] = {
    val z2 = this * this
    val s = new Complex(f.minus(f.one, z2.real), f.negate(z2.imag)).sqrt
    val l = new Complex(f.plus(s.real, f.negate(imag)), f.plus(s.imag, real)).log
    new Complex(l.imag, f.negate(l.real))
  }

  // atan(z) = (i/2) log((i + z)/(i - z))
  def atan: Complex[T] = {
    val n = new Complex(real, f.plus(imag, f.one))
    val d = new Complex(f.negate(real), f.minus(f.one, imag))
    val l = (n / d).log
    new Complex(f.div(imag, f.fromInt(-2)), f.div(real, f.fromInt(2)))
  }

  // exp(a+ci) = (exp(a) * cos(c)) + (exp(a) * sin(c))i
  def exp: Complex[T] = new Complex(
    f.times(t.exp(real), t.cos(imag)),
    f.times(t.exp(real), t.sin(imag))
  )

  // sin(a+ci) = (sin(a) * cosh(c)) + (-cos(a) * sinh(c))i
  def sin: Complex[T] = new Complex(
    f.times(t.sin(real), t.cosh(imag)),
    f.times(f.negate(t.cos(real)), t.sinh(imag))
  )

  // sinh(a+ci) = (sinh(a) * cos(c)) + (-cosh(a) * sin(c))i
  def sinh: Complex[T] = new Complex(
    f.times(t.sinh(real), t.cos(imag)),
    f.times(f.negate(t.cosh(real)), t.sin(imag))
  )

  // cos(a+ci) = (cos(a) * cosh(c)) + (-sin(a) * sinh(c))i 
  def cos: Complex[T] = new Complex(
    f.times(t.cos(real), t.cosh(imag)),
    f.times(f.negate(t.sin(real)), t.sinh(imag))
  )

  // cosh(a+ci) = (cosh(a) * cos(c)) + (-sinh(a) * sin(c))i 
  def cosh: Complex[T] = new Complex(
    f.times(t.cosh(real), t.cos(imag)),
    f.times(f.negate(t.sinh(real)), t.sin(imag))
  )

  // tan(a+ci) = (sin(a+a) + sinh(c+c)i) / (cos(a+a) + cosh(c+c))
  def tan: Complex[T] = {
    val r2 = f.plus(real, real)
    val i2 = f.plus(imag, imag)
    val d = f.plus(t.cos(r2), t.cosh(i2))
    new Complex(f.div(t.sin(r2), d), f.div(t.sinh(i2), d))
  }

  // tanh(a+ci) = (sinh(a+a) + sin(c+c)i) / (cosh(a+a) + cos(c+c))
  def tanh: Complex[T] = {
    val r2 = f.plus(real, real)
    val i2 = f.plus(imag, imag)
    val d = f.plus(t.cos(r2), t.cosh(i2))
    new Complex(f.div(t.sinh(r2), d), f.div(t.sin(i2), d))
  }
}


object FloatComplex {
  import FastComplex.{encode}

  final def apply(real: Float, imag: Float): FloatComplex =
    new FloatComplex(encode(real, imag))

  final def apply(real: Double, imag: Double) =
    new FloatComplex(encode(real.toFloat, imag.toFloat))

  def polar(magnitude: Float, angle: Float) =
    new FloatComplex(FastComplex.polar(magnitude, angle))

  final val i = new FloatComplex(4575657221408423936L)
  final val one = new FloatComplex(1065353216L)
  final val zero = new FloatComplex(0L)
}

/**
 * Value class which encodes two floating point values in a Long.
 *
 * We get (basically) unboxed complex numbers using this hack.
 * The underlying implementation lives in the FastComplex object.
 */
class FloatComplex(val u: Long) extends AnyVal {
  override final def toString: String = "(%s+%si)" format (real, imag)

  final def real: Float = FastComplex.real(u)
  final def imag: Float = FastComplex.imag(u)
  final def repr = "FloatComplex(%s, %s)" format(real, imag)
  final def abs: Float = FastComplex.abs(u)
  final def angle: Float = FastComplex.angle(u)
  final def conjugate = new FloatComplex(FastComplex.conjugate(u))
  final def isWhole: Boolean = FastComplex.isWhole(u)
  final def signum: Int = FastComplex.signum(u)
  final def complexSignum = new FloatComplex(FastComplex.complexSignum(u))
  final def negate = new FloatComplex(FastComplex.negate(u))

  final def +(b: FloatComplex) = new FloatComplex(FastComplex.add(u, b.u))
  final def -(b: FloatComplex) = new FloatComplex(FastComplex.subtract(u, b.u))
  final def *(b: FloatComplex) = new FloatComplex(FastComplex.multiply(u, b.u))
  final def /(b: FloatComplex) = new FloatComplex(FastComplex.divide(u, b.u))
  final def /~(b: FloatComplex) = new FloatComplex(FastComplex.quot(u, b.u))
  final def %(b: FloatComplex) = new FloatComplex(FastComplex.mod(u, b.u))

  final def /%(b: FloatComplex) = FastComplex.quotmod(u, b.u) match {
    case (q, m) => (new FloatComplex(q), new FloatComplex(m))
  }

  final def pow(b: FloatComplex) = new FloatComplex(FastComplex.pow(u, b.u))
  final def **(b: FloatComplex) = pow(b)

  final def pow(b: Int) = new FloatComplex(FastComplex.pow(u, FastComplex(b.toFloat, 0.0F)))
  final def **(b: Int) = pow(b)
}


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
  import java.lang.Math.{atan2, cos, sin, sqrt}

  // note the superstitious use of @inline and final everywhere

  final def apply(real: Float, imag: Float) = encode(real, imag)
  final def apply(real: Double, imag: Double) = encode(real.toFloat, imag.toFloat)

  // encode a float as some bits
  @inline final def bits(n: Float): Int = java.lang.Float.floatToRawIntBits(n)

  // decode some bits into a float
  @inline final def bits(n: Int): Float = java.lang.Float.intBitsToFloat(n)

  // get the real part of the complex number
  @inline final def real(d: Long): Float = bits((d & 0xffffffff).toInt)

  // get the imaginary part of the complex number
  @inline final def imag(d: Long): Float = bits((d >> 32).toInt)

  // define some handy constants
  final val i = encode(0.0F, 1.0F)
  final val one = encode(1.0F, 0.0F)
  final val zero = encode(0.0F, 0.0F)

  // encode two floats representing a complex number
  @inline final def encode(real: Float, imag: Float): Long = {
    (bits(imag).toLong << 32) + bits(real).toLong
  }

  // encode two floats representing a complex number in polar form
  @inline final def polar(magnitude: Float, angle: Float): Long = {
    encode(magnitude * cos(angle).toFloat, magnitude * sin(angle).toFloat)
  }

  // decode should be avoided in fast code because it allocates a Tuple2.
  final def decode(d: Long): (Float, Float) = (real(d), imag(d))

  // produces a string representation of the Long/(Float,Float)
  final def toRepr(d: Long): String = "FastComplex(%s -> %s)" format(d, decode(d))

  // get the magnitude/absolute value
  final def abs(d: Long): Float = {
    val re = real(d)
    val im = imag(d)
    sqrt(re * re + im * im).toFloat
  }

  // get the angle/argument
  final def angle(d: Long): Float = atan2(imag(d), real(d)).toFloat

  // get the complex conjugate
  final def conjugate(d: Long): Long = encode(real(d), -imag(d))

  // see if the complex number is a whole value
  final def isWhole(d: Long): Boolean = real(d) % 1.0F == 0.0F && imag(d) % 1.0F == 0.0F

  // get the sign of the complex number
  final def signum(d: Long): Int = real(d) compare 0.0F

  // get the complex sign of the complex number
  final def complexSignum(d: Long): Long = {
    val m = abs(d)
    if (m == 0.0F) zero else divide(d, encode(m, 0.0F))
  }

  // negation
  final def negate(a: Long): Long = encode(-real(a), -imag(a))

  // addition
  final def add(a: Long, b: Long): Long = encode(real(a) + real(b), imag(a) + imag(b))

  // subtraction
  final def subtract(a: Long, b: Long): Long = encode(real(a) - real(b), imag(a) - imag(b))

  // multiplication
  final def multiply(a: Long, b: Long): Long = {
    val re_a = real(a)
    val im_a = imag(a)
    val re_b = real(b)
    val im_b = imag(b)
    encode(re_a * re_b - im_a * im_b, im_a * re_b + re_a * im_b)
  }

  // division
  final def divide(a: Long, b: Long): Long = {
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

  final def quot(a: Long, b: Long): Long =
    encode(Math.floor(real(divide(a, b))).toFloat, 0.0F)

  final def mod(a: Long, b: Long): Long = subtract(a, multiply(b, quot(a, b)))

  final def quotmod(a: Long, b: Long): (Long, Long) = {
    val q = quot(a, b)
    (q, subtract(a, multiply(b, quot(a, b))))
  }

  // exponentiation
  final def pow(a: Long, b: Long): Long = if (b == zero) {
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

trait ComplexInstances {
  implicit def ComplexAlgebra[@spec(Float, Double) A: Fractional: Trig] =
    new ComplexAlgebra[A] {
      val f = Fractional[A]
      val t = Trig[A]
      def scalar = f
      def nroot = f
    }

  implicit def ComplexEq[A:Fractional] =
    new ComplexEq[A] {}

  implicit def ComplexIsSigned[A:Fractional:Trig] =
    new ComplexIsSigned[A] {
      val f = Fractional[A]
      val t = Trig[A]
    }
}

trait ComplexIsRing[@spec(Float, Double) A] extends Ring[Complex[A]] {
  implicit def f:Fractional[A]
  implicit def t:Trig[A]

  override def minus(a:Complex[A], b:Complex[A]): Complex[A] = a - b
  def negate(a:Complex[A]): Complex[A] = -a
  def one: Complex[A] = Complex.one(f, t)
  def plus(a:Complex[A], b:Complex[A]): Complex[A] = a + b
  override def pow(a:Complex[A], b:Int):Complex[A] = a.pow(b)
  override def times(a:Complex[A], b:Complex[A]): Complex[A] = a * b
  def zero: Complex[A] = Complex.zero(f, t)

  override def fromInt(n: Int): Complex[A] = Complex.fromInt[A](n)
}

trait ComplexIsEuclideanRing[@spec(Float,Double) A]
extends ComplexIsRing[A] with EuclideanRing[Complex[A]] {
  def quot(a:Complex[A], b:Complex[A]) = a /~ b
  def mod(a:Complex[A], b:Complex[A]) = a % b
  override def quotmod(a:Complex[A], b:Complex[A]) = a /% b
  def gcd(a:Complex[A], b:Complex[A]):Complex[A] =
    _gcd(a, b, Fractional[A])
  @tailrec
  private def _gcd(a:Complex[A], b:Complex[A], f:Fractional[A]):Complex[A] = {
    if (f.lt(a.abs, f.one)) {
      one
    } else if (b == zero) {
      a
    } else if (f.lt(b.abs, f.one)) {
      one
    } else {
      _gcd(b, a % b, f)
    }
  }
}

trait ComplexIsField[@spec(Float,Double) A]
extends ComplexIsEuclideanRing[A] with Field[Complex[A]] {
  override def fromDouble(n: Double): Complex[A] = Complex(f.fromDouble(n))
  def div(a:Complex[A], b:Complex[A]) = a / b
  def ceil(a:Complex[A]): Complex[A] = a.ceil
  def floor(a:Complex[A]): Complex[A] = a.floor
  def round(a:Complex[A]): Complex[A] = a.round
  def isWhole(a:Complex[A]) = a.isWhole
}

trait ComplexIsTrig[@spec(Float, Double) A] extends Trig[Complex[A]] {
  implicit def f: Fractional[A]
  implicit def t: Trig[A]

  def e: Complex[A] = new Complex[A](t.e, f.zero)
  def pi: Complex[A] = new Complex[A](t.pi, f.zero)

  def exp(a: Complex[A]): Complex[A] = a.exp
  def log(a: Complex[A]): Complex[A] = a.log

  def sin(a: Complex[A]): Complex[A] = a.sin
  def cos(a: Complex[A]): Complex[A] = a.cos
  def tan(a: Complex[A]): Complex[A] = a.tan

  def asin(a: Complex[A]): Complex[A] = a.sin
  def acos(a: Complex[A]): Complex[A] = a.cos
  def atan(a: Complex[A]): Complex[A] = a.tan
  def atan2(y: Complex[A], x: Complex[A]): Complex[A] =
    new Complex(x.real, y.imag).atan

  def sinh(x: Complex[A]): Complex[A] = x.sinh
  def cosh(x: Complex[A]): Complex[A] = x.cosh
  def tanh(x: Complex[A]): Complex[A] = x.tanh

  def toRadians(a: Complex[A]): Complex[A] = a
  def toDegrees(a: Complex[A]): Complex[A] = a
}

trait ComplexIsNRoot[A] extends NRoot[Complex[A]] {
  def nroot(a: Complex[A], k: Int): Complex[A] = a.nroot(k)
  override def sqrt(a: Complex[A]): Complex[A] = a.sqrt
  def fpow(a: Complex[A], b: Complex[A]): Complex[A] = a.pow(b)
}

trait ComplexEq[A] extends Eq[Complex[A]] {
  def eqv(x:Complex[A], y:Complex[A]) = x eqv y
  override def neqv(x:Complex[A], y:Complex[A]) = x neqv y
}

trait ComplexIsSigned[A] extends Signed[Complex[A]] {
  implicit def f:Fractional[A]
  implicit def t:Trig[A]

  def signum(a: Complex[A]): Int = a.signum
  def abs(a: Complex[A]): Complex[A] = Complex[A](a.abs, f.zero)
}

trait ComplexAlgebra[@spec(Float, Double) A] extends ComplexIsField[A]
with ComplexIsTrig[A] with ComplexIsNRoot[A]
with InnerProductSpace[Complex[A], A]
with FieldAlgebra[Complex[A], A] {
  def timesl(r: A, v: Complex[A]): Complex[A] = Complex(r, scalar.zero) * v
  def dot(x: Complex[A], y: Complex[A]): A =
    scalar.plus(scalar.times(x.real, y.real), scalar.times(x.imag, y.imag))
}
