package spire
package math

import scala.math.{ScalaNumber, ScalaNumericConversions}
import spire.algebra._
import spire.syntax.field._
import spire.syntax.isReal._
import spire.syntax.nroot._

object Quaternion extends QuaternionInstances {
  def i[@sp(Float, Double) A](implicit f: Rig[A]): Quaternion[A] =
    Quaternion(f.zero, f.one, f.zero, f.zero)
  def j[@sp(Float, Double) A](implicit f: Rig[A]): Quaternion[A] =
    Quaternion(f.zero, f.zero, f.one, f.zero)
  def k[@sp(Float, Double) A](implicit f: Rig[A]): Quaternion[A] =
    Quaternion(f.zero, f.zero, f.zero, f.one)

  def zero[@sp(Float, Double) A](implicit f: Semiring[A]): Quaternion[A] =
    Quaternion(f.zero, f.zero, f.zero, f.zero)
  def one[@sp(Float, Double) A](implicit f: Rig[A]): Quaternion[A] =
    Quaternion(f.one, f.zero, f.zero, f.zero)

  def apply[@sp(Float, Double) A](a: A)(implicit f: Semiring[A]): Quaternion[A] =
    Quaternion(a, f.zero, f.zero, f.zero)
  def apply[@sp(Float, Double) A](c: Complex[A])(implicit f: Semiring[A]): Quaternion[A] =
    Quaternion(c.real, c.imag, f.zero, f.zero)
}

// really a skew field
private[math] trait QuaternionAlgebra[A]
    extends Field[Quaternion[A]]
    with Eq[Quaternion[A]]
    with NRoot[Quaternion[A]]
    with InnerProductSpace[Quaternion[A], A]
    with FieldAlgebra[Quaternion[A], A] {

  implicit def f: Fractional[A]
  implicit def t: Trig[A]
  implicit def r: IsReal[A]

  def eqv(x: Quaternion[A], y: Quaternion[A]): Boolean = x == y
  override def neqv(x: Quaternion[A], y: Quaternion[A]): Boolean = x != y

  override def minus(a: Quaternion[A], b: Quaternion[A]): Quaternion[A] = a - b
  def negate(a: Quaternion[A]): Quaternion[A] = -a
  def one: Quaternion[A] = Quaternion.one[A]
  def plus(a: Quaternion[A], b: Quaternion[A]): Quaternion[A] = a + b
  override def pow(a: Quaternion[A], b: Int): Quaternion[A] = a.pow(b)
  override def times(a: Quaternion[A], b: Quaternion[A]): Quaternion[A] = a * b
  def zero: Quaternion[A] = Quaternion.zero[A]

  def div(a: Quaternion[A], b: Quaternion[A]): Quaternion[A] = a / b
  /* TODO: is it sound?
  def quot(a: Quaternion[A], b: Quaternion[A]): Quaternion[A] = a /~ b
  def mod(a: Quaternion[A], b: Quaternion[A]): Quaternion[A] = a % b
  def gcd(a: Quaternion[A], b: Quaternion[A]): Quaternion[A] = {
    @tailrec def _gcd(a: Quaternion[A], b: Quaternion[A]): Quaternion[A] =
      if (b.isZero) a else _gcd(b, a - (a / b).round * b)
    _gcd(a, b)
  }
   */

  def nroot(a: Quaternion[A], k: Int): Quaternion[A] = a.nroot(k)
  override def sqrt(a: Quaternion[A]): Quaternion[A] = a.sqrt
  def fpow(a: Quaternion[A], b: Quaternion[A]): Quaternion[A] = a.fpow(b.r) //FIXME

  def timesl(a: A, q: Quaternion[A]): Quaternion[A] = q * a
  def dot(x: Quaternion[A], y: Quaternion[A]): A = x.dot(y)
}

trait QuaternionInstances {
  implicit def QuaternionAlgebra[A](implicit fr: Fractional[A], tr: Trig[A], isr: IsReal[A]): QuaternionAlgebra[A] =
    new QuaternionAlgebra[A] {
      val f = fr
      val t = tr
      val r = isr
      def scalar = f
      def nroot = f
    }
}

final case class Quaternion[@sp(Float, Double) A](r: A, i: A, j: A, k: A)
    extends ScalaNumber with ScalaNumericConversions with Serializable { lhs =>

  // junky ScalaNumber stuff
  override def byteValue: Byte = longValue.toByte
  override def shortValue: Short = longValue.toShort
  def intValue: Int = longValue.toInt
  override def longValue: Long = anyToLong(r)
  def floatValue: Float = doubleValue.toFloat
  def doubleValue: Double = anyToDouble(r)

  private[this] def sillyIsReal: Boolean =
    anyIsZero(i) && anyIsZero(j) && anyIsZero(k)

  def underlying: Object = this

  def isWhole: Boolean =
    sillyIsReal && anyIsWhole(r)

  override final def isValidInt: Boolean =
    sillyIsReal && anyIsValidInt(r)

  // important to keep in sync with Complex[_]
  override def hashCode: Int =
    if (sillyIsReal) r.##
    else (19 * r.##) + (41 * i.##) + (13 * j.##) + (77 * k.##) + 97

  // not typesafe, so this is the best we can do :(
  override def equals(that: Any): Boolean = that match {
    case that: Quaternion[_] =>
      r == that.r && i == that.i && j == that.j && k == that.k
    case that: Complex[_] =>
      r == that.real && i == that.imag && anyIsZero(j) && anyIsZero(k)
    case that =>
      sillyIsReal && r == that
  }

  def isZero(implicit o: IsReal[A]): Boolean = r.isSignZero && i.isSignZero && j.isSignZero && k.isSignZero
  def isReal(implicit o: IsReal[A]): Boolean = i.isSignZero && j.isSignZero && k.isSignZero
  def isPure(implicit o: IsReal[A]): Boolean = r.isSignZero

  def real(implicit s: Semiring[A]): Quaternion[A] = Quaternion(r)
  def pure(implicit s: Semiring[A]): Quaternion[A] = Quaternion(s.zero, i, j, k)

  def abs(implicit f: Field[A], o: IsReal[A], n: NRoot[A]): A =
    (r.pow(2) + i.pow(2) + j.pow(2) + k.pow(2)).sqrt

  def pureAbs(implicit f: Field[A], o: IsReal[A], n: NRoot[A]): A =
    (i.pow(2) + j.pow(2) + k.pow(2)).sqrt

  def eqv(rhs: Quaternion[A])(implicit o: Eq[A]): Boolean =
    lhs.r === rhs.r && lhs.i === rhs.i && lhs.j === rhs.j && lhs.k === rhs.k

  def neqv(rhs: Quaternion[A])(implicit o: Eq[A]): Boolean =
    lhs.r =!= rhs.r && lhs.i =!= rhs.i && lhs.j =!= rhs.j && lhs.k =!= rhs.k

  override def toString: String = s"($r + ${i}i + ${j}j + ${k}k)"

  def toComplex: Complex[A] = Complex(r, i)

  def signum(implicit o: IsReal[A]): Int = r.signum match {
    case 0 => i.signum match {
      case 0 => j.signum match {
        case 0 => k.signum
        case n => n
      }
      case n => n
    }
    case n => n
  }

  def quaternionSignum(implicit f: Field[A], o: IsReal[A], n: NRoot[A]): Quaternion[A] =
    if (isZero) this else this / abs

  def pureSignum(implicit f: Field[A], o: IsReal[A], n: NRoot[A]): Quaternion[A] =
    if (isReal) Quaternion.zero[A] else (pure / pureAbs)

  def unary_-(implicit s: Rng[A]): Quaternion[A] =
    Quaternion(-r, -i, -j, -k)

  def conjugate(implicit s: Rng[A]): Quaternion[A] =
    Quaternion(r, -i, -j, -k)

  def reciprocal(implicit f: Field[A]): Quaternion[A] =
    conjugate / (r.pow(2) + i.pow(2) + j.pow(2) + k.pow(2))

  def sqrt(implicit f: Field[A], o: IsReal[A], n0: NRoot[A]): Quaternion[A] =
    if (!isReal) {
      val n = (r + abs).sqrt
      Quaternion(n, i / n, j / n, k / n) / f.fromInt(2).sqrt
    } else if (r.signum >= 0) {
      Quaternion(r.sqrt)
    } else {
      Quaternion(f.zero, r.abs.sqrt, f.zero, f.zero)
    }

  def nroot(m: Int)(implicit f: Field[A], o: IsReal[A], n0: NRoot[A], tr: Trig[A]): Quaternion[A] =
    if (m <= 0) {
      throw new IllegalArgumentException(s"illegal root: $m")
    } else if (m == 1) {
      this
    } else if (!isReal) {
      val s = pureAbs
      val n = abs
      val t = acos(r / n)
      val v = Quaternion(f.zero, i / s, j / s, k / s)
      val e = if (sin(t).signum >= 0) v else -v
      val tm = t / m
      (e * sin(tm) + cos(tm)) * n.nroot(m)
    } else if (r.signum >= 0) {
      Quaternion(r.nroot(m))
    } else {
      Quaternion(Complex(r).nroot(m))
    }

  def unit(implicit f: Field[A], o: IsReal[A], n: NRoot[A]): Quaternion[A] =
    Quaternion(r.pow(2), i.pow(2), j.pow(2), k.pow(2)) / abs

  def +(rhs: A)(implicit s: Semiring[A]): Quaternion[A] =
    Quaternion(r + rhs, i, j, k)
  def +(rhs: Complex[A])(implicit s: Semiring[A]): Quaternion[A] =
    Quaternion(r + rhs.real, i + rhs.imag, j, k)
  def +(rhs: Quaternion[A])(implicit s: Semiring[A]): Quaternion[A] =
    Quaternion(lhs.r + rhs.r, lhs.i + rhs.i, lhs.j + rhs.j, lhs.k + rhs.k)

  def -(rhs: A)(implicit s: Rng[A]): Quaternion[A] =
    Quaternion(r - rhs, i, j, k)
  def -(rhs: Complex[A])(implicit s: Rng[A]): Quaternion[A] =
    Quaternion(r - rhs.real, i - rhs.imag, j, k)
  def -(rhs: Quaternion[A])(implicit s: Rng[A]): Quaternion[A] =
    Quaternion(lhs.r - rhs.r, lhs.i - rhs.i, lhs.j - rhs.j, lhs.k - rhs.k)

  def *(rhs: A)(implicit s: Semiring[A]): Quaternion[A] =
    Quaternion(r * rhs, i * rhs, j * rhs, k * rhs)
  def *(rhs: Complex[A])(implicit s: Rng[A]): Quaternion[A] =
    Quaternion(
      (r * rhs.real) - (i * rhs.imag),
      (r * rhs.imag) + (i * rhs.real),
      (j * rhs.real) + (k * rhs.imag),
      (j * rhs.imag) + (k * rhs.real)
    )
  def *(rhs: Quaternion[A])(implicit s: Rng[A]): Quaternion[A] = Quaternion(
    (lhs.r * rhs.r) - (lhs.i * rhs.i) - (lhs.j * rhs.j) - (lhs.k * rhs.k),
    (lhs.r * rhs.i) + (lhs.i * rhs.r) + (lhs.j * rhs.k) - (lhs.k * rhs.j),
    (lhs.r * rhs.j) - (lhs.i * rhs.k) + (lhs.j * rhs.r) + (lhs.k * rhs.i),
    (lhs.r * rhs.k) + (lhs.i * rhs.j) - (lhs.j * rhs.i) + (lhs.k * rhs.r)
  )

  def /(rhs: A)(implicit f: Field[A]): Quaternion[A] =
    Quaternion(r / rhs, i / rhs, j / rhs, k / rhs)
  def /(rhs: Complex[A])(implicit f: Field[A]): Quaternion[A] =
    lhs * Quaternion(rhs).reciprocal
  def /(rhs: Quaternion[A])(implicit f: Field[A]): Quaternion[A] =
    lhs * rhs.reciprocal

  def pow(k: Int)(implicit s: Ring[A]): Quaternion[A] = {
    @tailrec def loop(p: Quaternion[A], b: Quaternion[A], e: Int): Quaternion[A] =
      if (e == 0) p
      else if ((e & 1) == 1) loop(p * b, b * b, e >>> 1)
      else loop(p, b * b, e >>> 1)

    if (k >= 0) loop(Quaternion.one[A], this, k)
    else throw new IllegalArgumentException(s"illegal exponent: $k")
  }

  def **(k: Int)(implicit s: Ring[A]): Quaternion[A] = pow(k)

  def fpow(k0: A)(implicit f: Field[A], o: IsReal[A], n0: NRoot[A], tr: Trig[A]): Quaternion[A] =
    if (k0.signum < 0) {
      Quaternion.zero
    } else if (k0 == f.zero) {
      Quaternion.one
    } else if (k0 == f.one) {
      this
    } else if (!isReal) {
      val s = (i ** 2 + j ** 2 + k ** 2).sqrt
      val v = Quaternion(f.zero, i / s, j / s, k / s)
      val n = abs
      val t = acos(r / n)
      (Quaternion(cos(t * k0)) + v * sin(t * k0)) * n.fpow(k0)
    } else if (r.signum >= 0) {
      Quaternion(r.fpow(k0))
    } else {
      Quaternion(Complex(r).pow(Complex(k0)))
    }

  def floor(implicit o: IsReal[A]): Quaternion[A] =
    Quaternion(r.floor, i.floor, j.floor, k.floor)

  def ceil(implicit o: IsReal[A]): Quaternion[A] =
    Quaternion(r.ceil, i.ceil, j.ceil, k.ceil)

  def round(implicit o: IsReal[A]): Quaternion[A] =
    Quaternion(r.round, i.round, j.round, k.round)

  // TODO: instead of floor for /~, should be round-toward-zero

  def /~(rhs: A)(implicit f: Field[A], o: IsReal[A]): Quaternion[A] =
    (lhs / rhs).floor
  def /~(rhs: Complex[A])(implicit f: Field[A], o: IsReal[A]): Quaternion[A] =
    (lhs / rhs).floor
  def /~(rhs: Quaternion[A])(implicit f: Field[A], o: IsReal[A]): Quaternion[A] =
    (lhs / rhs).floor

  def %(rhs: A)(implicit f: Field[A], o: IsReal[A]): Quaternion[A] =
    lhs - (lhs /~ rhs)
  def %(rhs: Complex[A])(implicit f: Field[A], o: IsReal[A]): Quaternion[A] =
    lhs - (lhs /~ rhs)
  def %(rhs: Quaternion[A])(implicit f: Field[A], o: IsReal[A]): Quaternion[A] =
    lhs - (lhs /~ rhs)

  def /%(rhs: A)(implicit f: Field[A], o: IsReal[A]): (Quaternion[A], Quaternion[A]) = {
    val q = lhs /~ rhs
    (q, lhs - q)
  }
  def /%(rhs: Complex[A])(implicit f: Field[A], o: IsReal[A]): (Quaternion[A], Quaternion[A]) = {
    val q = lhs /~ rhs
    (q, lhs - q)
  }
  def /%(rhs: Quaternion[A])(implicit f: Field[A], o: IsReal[A]): (Quaternion[A], Quaternion[A]) = {
    val q = lhs /~ rhs
    (q, lhs - q)
  }

  def dot(rhs: Quaternion[A])(implicit f: Field[A]): A =
    (lhs.conjugate * rhs + rhs.conjugate * lhs).r / f.fromInt(2)
}
