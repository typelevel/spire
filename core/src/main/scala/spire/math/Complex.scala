package spire.math

import spire.algebra._

import spire.syntax.field._
import spire.syntax.isReal._
import spire.syntax.nroot._
import spire.syntax.order._

import scala.{specialized => spec}
import scala.annotation.tailrec
import scala.math.{ScalaNumber, ScalaNumericConversions, ScalaNumericAnyConversions}
import java.lang.Math


object Complex extends ComplexInstances {
  def i[@spec(Float, Double) T](implicit T: Rig[T]) =
    new Complex(T.zero, T.one)

  def one[@spec(Float, Double) T](implicit T: Rig[T]) =
    new Complex(T.one, T.zero)

  def zero[@spec(Float, Double) T](implicit T: Semiring[T]) =
    new Complex(T.zero, T.zero)

  def fromInt[@spec(Float, Double) T](n: Int)(implicit f: Ring[T]) =
    new Complex(f.fromInt(n), f.zero)

  implicit def intToComplex(n: Int) = new Complex(n.toDouble, 0.0)
  implicit def longToComplex(n: Long) = new Complex(n.toDouble, 0.0)
  implicit def floatToComplex(n: Float) = new Complex(n, 0.0F)
  implicit def doubleToComplex(n: Double) = new Complex(n, 0.0)

  implicit def bigIntToComplex(n: BigInt): Complex[BigDecimal] =
    bigDecimalToComplex(BigDecimal(n))

  implicit def bigDecimalToComplex(n: BigDecimal): Complex[BigDecimal] = {
    implicit val mc = n.mc
    new Complex(n, BigDecimal(0))
  }

  def polar[@spec(Float, Double) T: Field: Trig](magnitude: T, angle: T): Complex[T] =
    new Complex(magnitude * Trig[T].cos(angle), magnitude * Trig[T].sin(angle))

  def apply[@spec(Float, Double) T: Semiring](real: T): Complex[T] =
    new Complex(real, Semiring[T].zero)

  def rootOfUnity[@spec(Float, Double) T](n: Int, x: Int)(implicit f: Field[T], t: Trig[T], r: IsReal[T]): Complex[T] = {
    if (x == 0) return one[T]

    if (n % 2 == 0) {
      if (x == n / 2) return -one[T]
      if (n % 4 == 0) {
        if (x == n / 4) return i[T]
        if (x == n * 3 / 4) return -i[T]
      }
    }

    polar(f.one, (t.pi * 2 * x) / n)
  }

  def rootsOfUnity[@spec(Float, Double) T](n: Int)(implicit f: Field[T], t: Trig[T], r: IsReal[T]): Array[Complex[T]] = {
    val roots = new Array[Complex[T]](n)
    var sum = one[T]
    roots(0) = sum

    val west = if (n % 2 == 0) n / 2 else -1
    val north = if (n % 4 == 0) n / 4 else -1
    val south = if (n % 4 == 0) 3 * n / 4 else -1

    var x = 1
    val last = n - 1
    while (x < last) {
      val c = x match {
        case `north` => i[T]
        case `west` => -one[T]
        case `south` => -i[T]
        case _ => polar(f.one, (t.pi * 2 * x) / n)
      }
      roots(x) = c
      sum += c
      x += 1
    }

    roots(last) = zero[T] - sum
    roots
  }
}

@SerialVersionUID(0L)
final case class Complex[@spec(Float, Double) T](real: T, imag: T)
    extends ScalaNumber with ScalaNumericConversions with Serializable { lhs =>

  import spire.syntax.order._

  /**
   * This returns the sign of `real` if it is not 0, otherwise it returns the
   * sign of `imag`.
   */
  def signum(implicit o: IsReal[T]): Int = real.signum match {
    case 0 => imag.signum
    case n => n
  }

  /**
   * This implements sgn(z), which (except for z=0) observes:
   *
   * `sgn(z) = z / abs(z) = abs(z) / z`
   */
  def complexSignum(implicit f: Field[T], o: IsReal[T], n: NRoot[T]): Complex[T] =
    if (isZero) this else this / abs

  def abs(implicit f: Field[T], o: IsReal[T], n: NRoot[T]): T =
    (real * real + imag * imag).sqrt

  def arg(implicit f: Field[T], t: Trig[T], o: IsReal[T]): T =
    if (isZero) f.zero else t.atan2(imag, real)

  def norm(implicit f: Field[T], n: NRoot[T], o: Order[T]): T = hypot(real, imag)

  def conjugate(implicit f: Rng[T]): Complex[T] = new Complex(real, -imag)

  def asTuple: (T, T) = (real, imag)
  def asPolarTuple(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): (T, T) = (abs, arg)

  def isZero(implicit o: IsReal[T]): Boolean = real.isSignZero && imag.isSignZero
  def isImaginary(implicit o: IsReal[T]): Boolean = real.isSignZero
  def isReal(implicit o: IsReal[T]): Boolean = imag.isSignZero

  def eqv(b: Complex[T])(implicit o: Eq[T]): Boolean = real === b.real && imag === b.imag
  def neqv(b: Complex[T])(implicit o: Eq[T]): Boolean = real =!= b.real || imag =!= b.imag

  def unary_-(implicit r: Rng[T]): Complex[T] = new Complex(-real, -imag)

  def +(rhs: T)(implicit r: Semiring[T]): Complex[T] = new Complex(real + rhs, imag)
  def -(rhs: T)(implicit r: Rng[T]): Complex[T] = new Complex(real - rhs, imag)
  def *(rhs: T)(implicit r: Semiring[T]): Complex[T] = new Complex(real * rhs, imag * rhs)
  def /(rhs: T)(implicit r: Field[T]): Complex[T] = new Complex(real / rhs, imag / rhs)

  // TODO: instead of floor should be round-toward-zero

  def /~(rhs: T)(implicit f: Field[T], o: IsReal[T]): Complex[T] = (this / rhs).floor
  def %(rhs: T)(implicit f: Field[T], o: IsReal[T]): Complex[T] = this - (this /~ rhs) * rhs
  def /%(rhs: T)(implicit f: Field[T], o: IsReal[T]): (Complex[T], Complex[T]) = {
    val q = this /~ rhs
    (q, this - q * rhs)
  }

  def **(e: T)(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = this pow e
  def pow(e: T)(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] =
    if (e.isSignZero) {
      Complex.one[T]
    } else if (this.isZero) {
      if (e < f.zero)
        throw new Exception("raising 0 to negative/complex power")
      Complex.zero[T]
    } else {
      Complex.polar(abs fpow e, arg * e)
    }

  def +(b: Complex[T])(implicit r: Semiring[T]): Complex[T] =
    new Complex(real + b.real, imag + b.imag)

  def -(b: Complex[T])(implicit r: Rng[T]): Complex[T] =
    new Complex(real - b.real, imag - b.imag)

  def *(b: Complex[T])(implicit r: Rng[T]): Complex[T] =
    new Complex(real * b.real - imag * b.imag, imag * b.real + real * b.imag)

  def /(b: Complex[T])(implicit f: Field[T], o: IsReal[T]): Complex[T] = {
    val abs_breal = b.real.abs
    val abs_bimag = b.imag.abs

    if (abs_breal >= abs_bimag) {
      if (abs_breal === f.zero) throw new Exception("/ by zero")
      val ratio = b.imag / b.real
      val denom = b.real + b.imag * ratio
      new Complex((real + imag * ratio) / denom, (imag - real * ratio) / denom)

    } else {
      if (abs_bimag === f.zero) throw new Exception("/ by zero")
      val ratio = b.real / b.imag
      val denom = b.real * ratio + b.imag
      new Complex((real * ratio + imag) / denom, (imag * ratio - real) /denom)
    }
  }

  def /~(b: Complex[T])(implicit f: Field[T], o: IsReal[T]): Complex[T] = {
    val d = this / b
    new Complex(d.real.floor, d.imag.floor)
  }

  def %(b: Complex[T])(implicit f: Field[T], o: IsReal[T]): Complex[T] = this - (this /~ b) * b

  def /%(b: Complex[T])(implicit f: Field[T], o: IsReal[T]): (Complex[T], Complex[T]) = {
    val q = this /~ b
    (q, this - q * b)
  }

  def **(b: Int)(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = pow(b)

  def nroot(k: Int)(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] =
    if (isZero) Complex.zero else pow(Complex(f.fromInt(k).reciprocal, f.zero))

  def pow(b: Int)(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] =
    if (isZero) Complex.zero else Complex.polar(abs.pow(b), arg * b)

  def **(b: Complex[T])(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = pow(b)

  def pow(b: Complex[T])(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] =
    if (b.isZero) {
      Complex.one[T]
    } else if (this.isZero) {
      if (b.imag =!= f.zero || b.real < f.zero)
        throw new Exception("raising 0 to negative/complex power")
      Complex.zero[T]
    } else if (b.imag =!= f.zero) {
      val len = (abs fpow b.real) / t.exp(arg * b.imag)
      val phase = arg * b.real + t.log(abs) * b.imag
      Complex.polar(len, phase)
    } else {
      Complex.polar(abs fpow b.real, arg * b.real)
    }

  // we are going with the "principal value" definition of Log.
  def log(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = {
    if (isZero) throw new IllegalArgumentException("log(0) undefined")
    new Complex(t.log(abs), arg)
  }

  def sqrt(implicit f: Field[T], n0: NRoot[T], o: IsReal[T]): Complex[T] = {
    if (isZero) {
      Complex.zero[T]
    } else {
      val two = f.fromInt(2)
      val a = ((abs + real.abs) / two).sqrt
      imag.signum match {
        case 0 =>
          if (real < f.zero) Complex(f.zero, a) else Complex(a, f.zero)
        case n =>
          val b = ((abs - real.abs) / two).sqrt
          if (n < 0) Complex(a, -b) else Complex(a, b)
      }
    }
  }

  def floor(implicit o: IsReal[T]): Complex[T] = new Complex(real.floor, imag.floor)
  def ceil(implicit o: IsReal[T]): Complex[T] = new Complex(real.ceil, imag.ceil)
  def round(implicit o: IsReal[T]): Complex[T] = new Complex(real.round, imag.round)

  // acos(z) = -i*(log(z + i*(sqrt(1 - z*z))))
  def acos(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = {
    val z2 = this * this
    val s = new Complex(f.one - z2.real, -z2.imag).sqrt
    val l = new Complex(real + s.imag, imag + s.real).log
    new Complex(l.imag, -l.real)
  }

  // asin(z) = -i*(log(sqrt(1 - z*z) + i*z))
  def asin(implicit f: Field[T], n: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = {
    val z2 = this * this
    val s = new Complex(f.one - z2.real, -z2.imag).sqrt
    val l = new Complex(s.real + -imag, s.imag + real).log
    new Complex(l.imag, -l.real)
  }

  // atan(z) = (i/2) log((i + z)/(i - z))
  def atan(implicit f: Field[T], r: NRoot[T], t: Trig[T], o: IsReal[T]): Complex[T] = {
    val n = new Complex(real, imag + f.one)
    val d = new Complex(-real, f.one - imag)
    val l = (n / d).log
    new Complex(l.imag / f.fromInt(-2), l.real / f.fromInt(2))
  }

  // exp(a+ci) = (exp(a) * cos(c)) + (exp(a) * sin(c))i
  def exp(implicit f: Field[T], t: Trig[T]): Complex[T] =
    new Complex(t.exp(real) * t.cos(imag), t.exp(real) * t.sin(imag))

  // sin(a+ci) = (sin(a) * cosh(c)) + (cos(a) * sinh(c))i
  def sin(implicit f: Field[T], t: Trig[T]): Complex[T] =
    new Complex(t.sin(real) * t.cosh(imag), t.cos(real) * t.sinh(imag))

  // sinh(a+ci) = (sinh(a) * cos(c)) + (cosh(a) * sin(c))i
  def sinh(implicit f: Field[T], t: Trig[T]): Complex[T] =
    new Complex(t.sinh(real) * t.cos(imag), t.cosh(real) * t.sin(imag))

  // cos(a+ci) = (cos(a) * cosh(c)) - (sin(a) * sinh(c))i
  def cos(implicit f: Field[T], t: Trig[T]): Complex[T] =
    new Complex(t.cos(real) * t.cosh(imag), -t.sin(real) * t.sinh(imag))

  // cosh(a+ci) = (cosh(a) * cos(c)) + (sinh(a) * sin(c))i
  def cosh(implicit f: Field[T], t: Trig[T]): Complex[T] =
    new Complex(t.cosh(real) * t.cos(imag), t.sinh(real) * t.sin(imag))

  // tan(a+ci) = (sin(a+a) + sinh(c+c)i) / (cos(a+a) + cosh(c+c))
  def tan(implicit f: Field[T], t: Trig[T]): Complex[T] = {
    val r2 = real + real
    val i2 = imag + imag
    val d = t.cos(r2) + t.cosh(i2)
    new Complex(t.sin(r2) / d, t.sinh(i2) / d)
  }

  // tanh(a+ci) = (sinh(a+a) + sin(c+c)i) / (cosh(a+a) + cos(c+c))
  def tanh(implicit f: Field[T], t: Trig[T]): Complex[T] = {
    val r2 = real + real
    val i2 = imag + imag
    val d = t.cos(r2) + t.cosh(i2)
    new Complex(t.sinh(r2) / d, t.sin(i2) / d)
  }

  // junky ScalaNumber stuff
  def floatValue: Float = doubleValue.toFloat
  def doubleValue: Double = anyToDouble(real)
  override def byteValue: Byte = longValue.toByte
  override def shortValue: Short = longValue.toShort
  def intValue: Int = longValue.toInt
  override def longValue: Long = anyToLong(real)

  def underlying: Object = this

  def isWhole: Boolean =
    anyIsZero(imag) && anyIsWhole(real)

  override final def isValidInt: Boolean =
    anyIsZero(imag) && anyIsValidInt(real)

  // important to keep in sync with Quaternion[_]
  override def hashCode: Int =
    if (anyIsZero(imag)) real.##
    else (19 * real.##) + (41 * imag.##) + 97

  // not typesafe, so this is the best we can do :(
  override def equals(that: Any): Boolean = that match {
    case that: Complex[_]    => ===(that)
    case that: Quaternion[_] =>
      real == that.r && imag == that.i && anyIsZero(that.j) && anyIsZero(that.k)
    case that =>
      anyIsZero(imag) && real == that
  }

  def ===(that: Complex[_]): Boolean =
    real == that.real && imag == that.imag

  override def toString: String = s"($real + ${imag}i)"

  def toQuaternion(implicit ev: AdditiveMonoid[T]): Quaternion[T] =
    Quaternion(real, imag, ev.zero, ev.zero)
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
  @inline final def imag(d: Long): Float = bits((d >>> 32).toInt)

  // define some handy constants
  final val i = encode(0.0F, 1.0F)
  final val one = encode(1.0F, 0.0F)
  final val zero = encode(0.0F, 0.0F)

  // encode two floats representing a complex number
  @inline final def encode(real: Float, imag: Float): Long =
    (bits(real) & 0xffffffffL) | ((bits(imag) & 0xffffffffL) << 32)

  // encode two floats representing a complex number in polar form
  @inline final def polar(magnitude: Float, angle: Float): Long =
    encode(magnitude * cos(angle).toFloat, magnitude * sin(angle).toFloat)

  // decode should be avoided in fast code because it allocates a Tuple2.
  final def decode(d: Long): (Float, Float) = (real(d), imag(d))

  // produces a string representation of the Long/(Float,Float)
  final def toRepr(d: Long): String = "FastComplex(%s -> %s)" format(d, decode(d))

  // get the magnitude/absolute value
  final def abs(d: Long): Float = {
    val re = real(d)
    val im = imag(d)
    java.lang.Math.sqrt(re * re + im * im).toFloat
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

    val abs_re_b = Math.abs(re_b)
    val abs_im_b = Math.abs(im_b)

    if (abs_re_b >= abs_im_b) {
      if (abs_re_b == 0.0F) throw new ArithmeticException("/0")
      val ratio = im_b / re_b
      val denom = re_b + im_b * ratio
      encode((re_a + im_a * ratio) / denom, (im_a - re_a * ratio) / denom)

    } else {
      if (abs_im_b == 0.0F) throw new ArithmeticException("/0")
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
    val len = (Math.pow(abs(a), re_b) / exp((angle(a) * im_b))).toFloat
    val phase = (angle(a) * re_b + log(abs(a)) * im_b).toFloat
    polar(len, phase)

  } else {
    val len = Math.pow(abs(a), real(b)).toFloat
    val phase = (angle(a) * real(b)).toFloat
    polar(len, phase)
  }
}

trait ComplexInstances0 {
  implicit def ComplexRing[A: Ring: IsReal]: Ring[Complex[A]] = new ComplexIsRingImpl[A]
}

trait ComplexInstances1 extends ComplexInstances0 {
  implicit def ComplexField[A: Field: IsReal]: Field[Complex[A]] = new ComplexIsFieldImpl[A]
}

trait ComplexInstances extends ComplexInstances1 {
  implicit def ComplexAlgebra[@spec(Float, Double) A: Fractional: Trig: IsReal] =
    new ComplexAlgebra[A]

  implicit def ComplexEq[A: Eq]: Eq[Complex[A]] = new ComplexEq[A]
}

private[math] trait ComplexIsRing[@spec(Float, Double) A] extends Ring[Complex[A]] {
  implicit def algebra: Ring[A]
  implicit def order: IsReal[A]

  override def minus(a: Complex[A], b: Complex[A]): Complex[A] = a - b
  def negate(a: Complex[A]): Complex[A] = -a
  def one: Complex[A] = Complex.one
  def plus(a: Complex[A], b: Complex[A]): Complex[A] = a + b
  override def times(a: Complex[A], b: Complex[A]): Complex[A] = a * b
  def zero: Complex[A] = Complex.zero

  override def fromInt(n: Int): Complex[A] = Complex.fromInt[A](n)
}

private[math] trait ComplexIsField[@spec(Float,Double) A] extends ComplexIsRing[A] with Field[Complex[A]] {

  implicit def algebra: Field[A]

  override def fromDouble(n: Double): Complex[A] = Complex(algebra.fromDouble(n))
  def div(a: Complex[A], b: Complex[A]) = a / b
  def quot(a: Complex[A], b: Complex[A]) = a /~ b
  def mod(a: Complex[A], b: Complex[A]) = a % b
  override def quotmod(a: Complex[A], b: Complex[A]) = a /% b
  def gcd(a: Complex[A], b: Complex[A]): Complex[A] = {
    @tailrec def _gcd(a: Complex[A], b: Complex[A]): Complex[A] =
      if (b.isZero) a else _gcd(b, a - (a / b).round * b)
    _gcd(a, b)
  }
}

private[math] trait ComplexIsTrig[@spec(Float, Double) A] extends Trig[Complex[A]] {
  implicit def algebra: Field[A]
  implicit def nroot: NRoot[A]
  implicit def trig: Trig[A]
  implicit def order: IsReal[A]

  def e: Complex[A] = new Complex[A](trig.e, algebra.zero)
  def pi: Complex[A] = new Complex[A](trig.pi, algebra.zero)

  def exp(a: Complex[A]): Complex[A] = a.exp
  def expm1(a: Complex[A]): Complex[A] = a.exp - algebra.one
  def log(a: Complex[A]): Complex[A] = a.log
  def log1p(a: Complex[A]): Complex[A] = (a + algebra.one).log

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

private[math] trait ComplexIsNRoot[A] extends NRoot[Complex[A]] {
  implicit def algebra: Field[A]
  implicit def nroot: NRoot[A]
  implicit def trig: Trig[A]
  implicit def order: IsReal[A]

  def nroot(a: Complex[A], k: Int): Complex[A] = a.nroot(k)
  override def sqrt(a: Complex[A]): Complex[A] = a.sqrt
  def fpow(a: Complex[A], b: Complex[A]): Complex[A] = a.pow(b)
}

private[math] trait ComplexIsSigned[A] extends Signed[Complex[A]] {
  implicit def algebra: Field[A]
  implicit def nroot: NRoot[A]
  implicit def order: IsReal[A]

  def signum(a: Complex[A]): Int = a.signum
  def abs(a: Complex[A]): Complex[A] = Complex[A](a.abs, algebra.zero)
}

@SerialVersionUID(1L)
private[math] class ComplexEq[A: Eq] extends Eq[Complex[A]] with Serializable {
  def eqv(x: Complex[A], y: Complex[A]) = x eqv y
  override def neqv(x: Complex[A], y: Complex[A]) = x neqv y
}

@SerialVersionUID(1L)
private[math] final class ComplexIsRingImpl[@spec(Float,Double) A](implicit
    val algebra: Ring[A], val order: IsReal[A]) extends ComplexIsRing[A] with Serializable

@SerialVersionUID(1L)
private[math] final class ComplexIsFieldImpl[@spec(Float,Double) A](implicit
    val algebra: Field[A], val order: IsReal[A]) extends ComplexIsField[A] with Serializable

@SerialVersionUID(1L)
private[math] class ComplexAlgebra[@spec(Float, Double) A](implicit
      val algebra: Field[A], val nroot: NRoot[A], val trig: Trig[A], val order: IsReal[A])
    extends ComplexIsField[A]
    with ComplexIsTrig[A]
    with ComplexIsNRoot[A]
    with ComplexIsSigned[A]
    with InnerProductSpace[Complex[A], A]
    with FieldAlgebra[Complex[A], A]
    with Serializable {
  def scalar = algebra
  def timesl(a: A, v: Complex[A]): Complex[A] = Complex(a, scalar.zero) * v
  def dot(x: Complex[A], y: Complex[A]): A =
    scalar.plus(scalar.times(x.real, y.real), scalar.times(x.imag, y.imag))
  override def pow(a: Complex[A], b: Int): Complex[A] = a.pow(b)
}
