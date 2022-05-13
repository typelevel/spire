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

import spire.algebra._

import spire.syntax.field._
import spire.syntax.isReal._
import spire.syntax.nroot._

import scala.math.{ScalaNumber, ScalaNumericConversions}

object Complex extends ComplexInstances {
  def i[@sp(Float, Double) T](implicit T: CRing[T]): Complex[T] =
    new Complex(T.zero, T.one)

  def one[@sp(Float, Double) T](implicit T: CRing[T]): Complex[T] =
    new Complex(T.one, T.zero)

  def zero[@sp(Float, Double) T](implicit T: CRing[T]): Complex[T] =
    new Complex(T.zero, T.zero)

  def fromInt[@sp(Float, Double) T](n: Int)(implicit f: CRing[T]): Complex[T] =
    new Complex(f.fromInt(n), f.zero)

  implicit def intToComplex(n: Int): Complex[Double] = new Complex(n.toDouble, 0.0)
  implicit def longToComplex(n: Long): Complex[Double] = new Complex(n.toDouble, 0.0)
  implicit def floatToComplex(n: Float): Complex[Float] = new Complex(n, 0.0f)
  implicit def doubleToComplex(n: Double): Complex[Double] = new Complex(n, 0.0)

  implicit def bigIntToComplex(n: BigInt): Complex[BigDecimal] =
    bigDecimalToComplex(BigDecimal(n))

  implicit def bigDecimalToComplex(n: BigDecimal): Complex[BigDecimal] = {
    new Complex(n, BigDecimal(0))
  }

  def polar[@sp(Float, Double) T: Field: Trig](magnitude: T, angle: T): Complex[T] =
    new Complex(magnitude * Trig[T].cos(angle), magnitude * Trig[T].sin(angle))

  def apply[@sp(Float, Double) T: CRing](real: T): Complex[T] =
    new Complex(real, CRing[T].zero)

  def rootOfUnity[@sp(Float, Double) T](n: Int, x: Int)(implicit f: Field[T], t: Trig[T]): Complex[T] = {
    if (x == 0) return one[T]

    if (n % 2 == 0) {
      if (x == n / 2) return -one[T]
      if (n % 4 == 0) {
        if (x == n / 4) return i[T]
        if (x == n * 3 / 4) return -i[T]
      }
    }

    polar(f.one, t.pi * 2 * x / n)
  }

  def rootsOfUnity[@sp(Float, Double) T](n: Int)(implicit f: Field[T], t: Trig[T]): Array[Complex[T]] = {
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
        case `west`  => -one[T]
        case `south` => -i[T]
        case _       => polar(f.one, t.pi * 2 * x / n)
      }
      roots(x) = c
      sum += c
      x += 1
    }

    roots(last) = zero[T] - sum
    roots
  }
}

/**
 * Complex numbers. Depending on the underlying scalar T, can represent the Gaussian integers (T = BigInt/SafeLong), the
 * Gaussian rationals (T = Rational) or the complex number field (T: Field).
 *
 * Note that we require T to be at least CRing, a commutative ring, so the implementation below is slightly less general
 * than the Cayley-Dickson construction.
 */
@SerialVersionUID(0L)
final case class Complex[@sp(Float, Double) T](real: T, imag: T)
    extends ScalaNumber
    with ScalaNumericConversions
    with Serializable { lhs =>

  import spire.syntax.order._

  /**
   * This implements sgn(z), which (except for z=0) observes:
   *
   * `sgn(z) = z / abs(z) = abs(z) / z`
   */
  def complexSignum(implicit f: Field[T], n: NRoot[T], o: Order[T], s: Signed[T]): Complex[T] =
    if (isZero) this else this / abs

  def abs(implicit f: Field[T], n: NRoot[T], o: Order[T], s: Signed[T]): T = hypot(real, imag)

  def absSquare(implicit r: CRing[T]): T = real * real + imag * imag

  def arg(implicit f: Field[T], s: Signed[T], t: Trig[T]): T =
    if (isZero) f.zero else t.atan2(imag, real)

  def norm(implicit f: Field[T], n: NRoot[T], o: Order[T], s: Signed[T]): T = hypot(real, imag)

  def conjugate(implicit f: CRing[T]): Complex[T] = new Complex(real, -imag)

  def asTuple: (T, T) = (real, imag)
  def asPolarTuple(implicit f: Field[T], n: NRoot[T], o: Order[T], s: Signed[T], t: Trig[T]): (T, T) = (abs, arg)

  def isZero(implicit s: Signed[T]): Boolean = real.isSignZero && imag.isSignZero
  def isImaginary(implicit s: Signed[T]): Boolean = real.isSignZero
  def isReal(implicit s: Signed[T]): Boolean = imag.isSignZero

  def eqv(b: Complex[T])(implicit o: Eq[T]): Boolean = real === b.real && imag === b.imag
  def neqv(b: Complex[T])(implicit o: Eq[T]): Boolean = real =!= b.real || imag =!= b.imag

  def unary_-(implicit r: CRing[T]): Complex[T] = new Complex(-real, -imag)

  def +(rhs: T)(implicit r: CRing[T]): Complex[T] = new Complex(real + rhs, imag)
  def -(rhs: T)(implicit r: CRing[T]): Complex[T] = new Complex(real - rhs, imag)
  def *(rhs: T)(implicit r: CRing[T]): Complex[T] = new Complex(real * rhs, imag * rhs)
  def /(rhs: T)(implicit r: Field[T]): Complex[T] = new Complex(real / rhs, imag / rhs)

  /* TODO: does it make sense? Should match the behavior on Gaussian integers.
  // TODO: instead of floor should be round-toward-zero

  def /~(rhs: T)(implicit f: Field[T], o: IsReal[T]): Complex[T] = (this / rhs).floor
  def %(rhs: T)(implicit f: Field[T], o: IsReal[T]): Complex[T] = this - (this /~ rhs) * rhs
  def /%(rhs: T)(implicit f: Field[T], o: IsReal[T]): (Complex[T], Complex[T]) = {
    val q = this /~ rhs
    (q, this - q * rhs)
  }
   */

  def **(e: T)(implicit f: Field[T], n: NRoot[T], o: Order[T], s: Signed[T], t: Trig[T]): Complex[T] = this.pow(e)
  def pow(e: T)(implicit f: Field[T], n: NRoot[T], o: Order[T], s: Signed[T], t: Trig[T]): Complex[T] =
    if (e.isSignZero) {
      Complex.one[T]
    } else if (this.isZero) {
      if (e < f.zero)
        throw new Exception("raising 0 to negative/complex power")
      Complex.zero[T]
    } else {
      Complex.polar(abs.fpow(e), arg * e)
    }

  def +(b: Complex[T])(implicit r: CRing[T]): Complex[T] =
    new Complex(real + b.real, imag + b.imag)

  def -(b: Complex[T])(implicit r: CRing[T]): Complex[T] =
    new Complex(real - b.real, imag - b.imag)

  def *(b: Complex[T])(implicit r: CRing[T]): Complex[T] =
    new Complex(real * b.real - imag * b.imag, imag * b.real + real * b.imag)

  def /(b: Complex[T])(implicit f: Field[T], o: Order[T], s: Signed[T]): Complex[T] = {
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
      new Complex((real * ratio + imag) / denom, (imag * ratio - real) / denom)
    }
  }

  /* TODO: does it make sense? Should match the behavior on Gaussian integers.
  def /~(b: Complex[T])(implicit f: Field[T], o: IsReal[T]): Complex[T] = {
    val d = this / b
    new Complex(d.real.floor, d.imag.floor)
  }

  def %(b: Complex[T])(implicit f: Field[T], o: IsReal[T]): Complex[T] = this - (this /~ b) * b

  def /%(b: Complex[T])(implicit f: Field[T], o: IsReal[T]): (Complex[T], Complex[T]) = {
    val q = this /~ b
    (q, this - q * b)
  }
   */

  def **(b: Int)(implicit f: Field[T], n: NRoot[T], o: Order[T], s: Signed[T], t: Trig[T]): Complex[T] = pow(b)

  def nroot(k: Int)(implicit f: Field[T], n: NRoot[T], o: Order[T], s: Signed[T], t: Trig[T]): Complex[T] =
    if (isZero) Complex.zero else pow(Complex(f.fromInt(k).reciprocal, f.zero))

  def pow(b: Int)(implicit f: Field[T], n: NRoot[T], o: Order[T], s: Signed[T], t: Trig[T]): Complex[T] =
    if (isZero) Complex.zero else Complex.polar(abs.pow(b), arg * b)

  def **(b: Complex[T])(implicit f: Field[T], n: NRoot[T], o: Order[T], s: Signed[T], t: Trig[T]): Complex[T] = pow(b)

  def pow(b: Complex[T])(implicit f: Field[T], n: NRoot[T], o: Order[T], s: Signed[T], t: Trig[T]): Complex[T] =
    if (b.isZero) {
      Complex.one[T]
    } else if (this.isZero) {
      if (b.imag =!= f.zero || b.real < f.zero)
        throw new Exception("raising 0 to negative/complex power")
      Complex.zero[T]
    } else if (b.imag =!= f.zero) {
      val len = abs.fpow(b.real) / t.exp(arg * b.imag)
      val phase = arg * b.real + t.log(abs) * b.imag
      Complex.polar(len, phase)
    } else {
      Complex.polar(abs.fpow(b.real), arg * b.real)
    }

  // we are going with the "principal value" definition of Log.
  def log(implicit f: Field[T], n: NRoot[T], o: Order[T], t: Trig[T], s: Signed[T]): Complex[T] = {
    if (isZero) throw new IllegalArgumentException("log(0) undefined")
    new Complex(t.log(abs), arg)
  }

  // returns the root with angle in (-pi/2, pi/2]
  def sqrt(implicit f: Field[T], n0: NRoot[T], o: Order[T], s: Signed[T]): Complex[T] = {
    if (isZero) {
      this
    } else if (imag.isSignZero) {
      if (real.isSignNegative)
        Complex(f.zero, real.abs.sqrt)
      else
        Complex(real.abs.sqrt, f.zero)
    } else {
      // https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Negative_or_complex_square
      val two = f.fromInt(2)
      val abs = this.abs
      val a = ((abs + real) / two).sqrt
      val b = ((abs - real) / two).sqrt
      if (imag.isSignNegative)
        Complex(a, -b)
      else
        Complex(a, b)
    }
  }

  def floor(implicit o: IsReal[T]): Complex[T] = new Complex(real.floor, imag.floor)
  def ceil(implicit o: IsReal[T]): Complex[T] = new Complex(real.ceil, imag.ceil)
  def round(implicit o: IsReal[T]): Complex[T] = new Complex(real.round, imag.round)

  // acos(z) = -i*(log(z + i*(sqrt(1 - z*z))))
  def acos(implicit f: Field[T], n: NRoot[T], o: Order[T], t: Trig[T], s0: Signed[T]): Complex[T] = {
    val z2 = this * this
    val s = new Complex(f.one - z2.real, -z2.imag).sqrt
    val l = new Complex(real + s.imag, imag + s.real).log
    new Complex(l.imag, -l.real)
  }

  // asin(z) = -i*(log(sqrt(1 - z*z) + i*z))
  def asin(implicit f: Field[T], n: NRoot[T], o: Order[T], t: Trig[T], s0: Signed[T]): Complex[T] = {
    val z2 = this * this
    val s = new Complex(f.one - z2.real, -z2.imag).sqrt
    val l = new Complex(s.real + -imag, s.imag + real).log
    new Complex(l.imag, -l.real)
  }

  // atan(z) = (i/2) log((i + z)/(i - z))
  def atan(implicit f: Field[T], o: Order[T], r: NRoot[T], s: Signed[T], t: Trig[T]): Complex[T] = {
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

  final override def isValidInt: Boolean =
    anyIsZero(imag) && anyIsValidInt(real)

  // important to keep in sync with Quaternion[_]
  override def hashCode: Int =
    if (anyIsZero(imag)) real.##
    else 19 * real.## + 41 * imag.## + 97

  // not typesafe, so this is the best we can do :(
  override def equals(that: Any): Boolean = that match {
    case that: Complex[_] => this === that
    case that: Quaternion[_] =>
      real == that.r && imag == that.i && anyIsZero(that.j) && anyIsZero(that.k)
    case that =>
      anyIsZero(imag) && real == that
  }

  def ===(that: Complex[_]): Boolean =
    real == that.real && imag == that.imag

  def =!=(that: Complex[_]): Boolean =
    !(this === that)

  override def toString: String = s"($real + ${imag}i)"

  def toQuaternion(implicit ev: AdditiveMonoid[T]): Quaternion[T] =
    Quaternion(real, imag, ev.zero, ev.zero)
}

trait ComplexInstances0 {
  implicit def ComplexOnCRing[A: CRing: Signed]: ComplexOnCRing[A] = new ComplexOnCRingImpl[A]
}

trait ComplexInstances1 extends ComplexInstances0 {
  implicit def ComplexOnField[A: Field: Order: Signed]: ComplexOnField[A] = new ComplexOnFieldImpl[A]
}

trait ComplexInstances extends ComplexInstances1 {
  implicit def ComplexOnTrig[@sp(Float, Double) A: Fractional: Order: Trig: Signed]: ComplexOnTrigImpl[A] =
    new ComplexOnTrigImpl[A]

  implicit def ComplexEq[A: Eq]: Eq[Complex[A]] = new ComplexEq[A]
}

private[math] trait ComplexOnCRing[@sp(Float, Double) A]
    extends CRing[Complex[A]]
    with RingAssociativeAlgebra[Complex[A], A]
    with Involution[Complex[A]] {
  implicit def scalar: CRing[A]
  override def minus(a: Complex[A], b: Complex[A]): Complex[A] = a - b
  def negate(a: Complex[A]): Complex[A] = -a
  def one: Complex[A] = Complex.one
  def plus(a: Complex[A], b: Complex[A]): Complex[A] = a + b
  override def times(a: Complex[A], b: Complex[A]): Complex[A] = a * b
  def timesl(a: A, v: Complex[A]): Complex[A] = Complex(a, scalar.zero) * v
  def zero: Complex[A] = Complex.zero

  def adjoint(a: Complex[A]): Complex[A] = a.conjugate

  override def fromInt(n: Int): Complex[A] = Complex.fromInt[A](n)
}

private[math] trait ComplexOnField[@sp(Float, Double) A]
    extends ComplexOnCRing[A]
    with Field[Complex[A]]
    with FieldAssociativeAlgebra[Complex[A], A] {

  implicit def scalar: Field[A]
  implicit def signed: Signed[A]
  implicit def order: Order[A]

  override def fromDouble(n: Double): Complex[A] = Complex(scalar.fromDouble(n))
  def div(a: Complex[A], b: Complex[A]): Complex[A] = a / b
}

private[math] trait ComplexOnTrig[@sp(Float, Double) A] extends Trig[Complex[A]] {
  implicit def scalar: Field[A]
  implicit def nroot: NRoot[A]
  implicit def order: Order[A]
  implicit def trig: Trig[A]
  implicit def signed: Signed[A]

  def e: Complex[A] = new Complex[A](trig.e, scalar.zero)
  def pi: Complex[A] = new Complex[A](trig.pi, scalar.zero)

  def exp(a: Complex[A]): Complex[A] = a.exp
  def expm1(a: Complex[A]): Complex[A] = a.exp - scalar.one
  def log(a: Complex[A]): Complex[A] = a.log
  def log1p(a: Complex[A]): Complex[A] = (a + scalar.one).log

  def sin(a: Complex[A]): Complex[A] = a.sin
  def cos(a: Complex[A]): Complex[A] = a.cos
  def tan(a: Complex[A]): Complex[A] = a.tan

  def asin(a: Complex[A]): Complex[A] = a.asin
  def acos(a: Complex[A]): Complex[A] = a.acos
  def atan(a: Complex[A]): Complex[A] = a.atan
  def atan2(y: Complex[A], x: Complex[A]): Complex[A] =
    new Complex(x.real, y.imag).atan

  def sinh(x: Complex[A]): Complex[A] = x.sinh
  def cosh(x: Complex[A]): Complex[A] = x.cosh
  def tanh(x: Complex[A]): Complex[A] = x.tanh

  def toRadians(a: Complex[A]): Complex[A] = a
  def toDegrees(a: Complex[A]): Complex[A] = a
}

private[math] trait ComplexIsNRoot[A] extends NRoot[Complex[A]] {
  implicit def scalar: Field[A]
  implicit def nroot: NRoot[A]
  implicit def order: Order[A]
  implicit def trig: Trig[A]
  implicit def signed: Signed[A]

  def nroot(a: Complex[A], k: Int): Complex[A] = a.nroot(k)
  override def sqrt(a: Complex[A]): Complex[A] = a.sqrt
  def fpow(a: Complex[A], b: Complex[A]): Complex[A] = a.pow(b)
}

@SerialVersionUID(1L)
private[math] class ComplexEq[A: Eq] extends Eq[Complex[A]] with Serializable {
  def eqv(x: Complex[A], y: Complex[A]): Boolean = x.eqv(y)
  override def neqv(x: Complex[A], y: Complex[A]): Boolean = x.neqv(y)
}

@SerialVersionUID(1L)
final private[math] class ComplexOnCRingImpl[@sp(Float, Double) A](implicit val scalar: CRing[A])
    extends ComplexOnCRing[A]
    with Serializable

@SerialVersionUID(1L)
final private[math] class ComplexOnFieldImpl[@sp(Float, Double) A](implicit
  val scalar: Field[A],
  val order: Order[A],
  val signed: Signed[A]
) extends ComplexOnField[A]
    with Serializable

@SerialVersionUID(1L)
private[math] class ComplexOnTrigImpl[@sp(Float, Double) A](implicit
  val scalar: Field[A],
  val nroot: NRoot[A],
  val order: Order[A],
  val trig: Trig[A],
  val signed: Signed[A]
) extends ComplexOnField[A]
    with ComplexOnTrig[A]
    with ComplexIsNRoot[A]
    with Serializable {
  override def pow(a: Complex[A], b: Int): Complex[A] = a.pow(b)
}
