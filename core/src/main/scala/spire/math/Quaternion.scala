package spire.math

import spire.algebra._
import spire.std.float._
import spire.std.double._
import spire.std.bigDecimal._

import spire.syntax.convertableFrom._
import spire.syntax.field._
import spire.syntax.isReal._
import spire.syntax.nroot._

import scala.{specialized => spec}
import scala.annotation.tailrec
import scala.math.{ScalaNumber, ScalaNumericConversions}
import java.lang.Math

object Quaternion {
  def i[@spec(Float, Double) A](implicit f: Fractional[A], trig: Trig[A], isr: IsReal[A]): Quaternion[A] =
    Quaternion(f.zero, f.one, f.zero, f.zero)
  def j[@spec(Float, Double) A](implicit f: Fractional[A], trig: Trig[A], isr: IsReal[A]): Quaternion[A] =
    Quaternion(f.zero, f.zero, f.one, f.zero)
  def k[@spec(Float, Double) A](implicit f: Fractional[A], trig: Trig[A], isr: IsReal[A]): Quaternion[A] =
    Quaternion(f.zero, f.zero, f.zero, f.one)

  def zero[@spec(Float, Double) A](implicit f: Fractional[A], trig: Trig[A], isr: IsReal[A]): Quaternion[A] =
    Quaternion(f.zero, f.zero, f.zero, f.zero)
  def one[@spec(Float, Double) A](implicit f: Fractional[A], trig: Trig[A], isr: IsReal[A]): Quaternion[A] =
    Quaternion(f.one, f.zero, f.zero, f.zero)

  def apply[@spec(Float, Double) A](n: A)(implicit f: Fractional[A], trig: Trig[A], isr: IsReal[A]): Quaternion[A] =
    Quaternion(n, f.zero, f.zero, f.zero)
  def apply[@spec(Float, Double) A](c: Complex[A])(implicit f: Fractional[A], trig: Trig[A], isr: IsReal[A]): Quaternion[A] =
    Quaternion(c.real, c.imag, f.zero, f.zero)
}

final case class Quaternion[@spec(Float, Double) A](r: A, i: A, j: A, k: A)
  (implicit f: Fractional[A], trig: Trig[A], isr: IsReal[A])
    extends ScalaNumber with ScalaNumericConversions with Serializable { lhs =>

  import f.{zero, one, fromInt => int, fromDouble => double}
  
  def doubleValue: Double = if (isReal) r.toDouble else Double.NaN
  def floatValue: Float = if (isReal) r.toFloat else Float.NaN
  def longValue: Long = r.toLong
  def intValue: Int = r.toInt

  def isReal: Boolean = i === zero && j === zero && k === zero
  def isPure: Boolean = r === zero
  def isWhole: Boolean = isReal && r.isWhole

  def real: Quaternion[A] = Quaternion(r, zero, zero, zero)
  def unreal: Quaternion[A] = Quaternion(zero, i, j, k)

  override def isValidInt: Boolean =
    i === zero && j === zero && k === zero && r.isWhole &&
    r <= int(Int.MaxValue) && r >= int(Int.MinValue)

  def underlying = (r, i, j, k)

  // important to keep in sync with Complex[_]
  override def hashCode: Int =
    if (isValidInt) r.toInt
    else 19 * r.## + 41 * i.## + 13 * j.## + 77 * k.## + 97

  override def equals(that: Any): Boolean = that match {
    case that: Quaternion[_] =>
      r == that.r && i == that.i && j == that.j && k == that.k
    case that: Complex[_] =>
      r == that.real && i == that.imag && j == zero && k == zero
    case that =>
      unifiedPrimitiveEquals(that)
  }

  override def toString: String = s"($r + ${i}i + ${j}j + ${k}k)"

  def unary_-(): Quaternion[A] =
    Quaternion(-r, -i, -j, -k)

  def conjugate(): Quaternion[A] =
    Quaternion(r, -i, -j, -k)

  def norm(): A =
    (r ** 2 + i ** 2 + j ** 2 + k ** 2).sqrt

  def reciprocal(): Quaternion[A] =
    conjugate / (r ** 2 + i ** 2 + j ** 2 + k ** 2)

  def sqrt(): Quaternion[A] =
    if (!isReal) {
      val n = (r + norm).sqrt
      Quaternion(n, i / n, j / n, k / n) / double(2.0).sqrt
    } else if (r >= zero) {
      Quaternion(r.sqrt)
    } else {
      Quaternion(zero, r.abs.sqrt, zero, zero)
    }

  def nroot(k0: Int): Quaternion[A] =
    if (k0 <= 0) {
      sys.error(s"illegal root: $k0")
    } else if (k0 == 1) {
      this
    } else if (!isReal) {
      val s = (i ** 2 + j ** 2 + k ** 2).sqrt
      val v = Quaternion(zero, i / s, j / s, k / s)
      val n = norm
      val t = acos(r / n)
      (Quaternion(cos(t / k0)) + v * sin(t / k0)) * n.nroot(k0)
    } else if (((k0 & 1) == 1) ^ (r >= 0)) {
      Quaternion(r.abs.nroot(k0))
    } else {
      Quaternion(zero, r.abs.nroot(k0), zero, zero)
    }

  def unit(): Quaternion[A] =
    Quaternion(r ** 2, i ** 2, j ** 2, k ** 2) / norm

  def +(rhs: A): Quaternion[A] =
    Quaternion(r + rhs, i, j, k)
  def +(rhs: Complex[A]): Quaternion[A] =
    Quaternion(r + rhs.real, i + rhs.imag, j, k)
  def +(rhs: Quaternion[A]): Quaternion[A] =
    Quaternion(lhs.r + rhs.r, lhs.i + rhs.i, lhs.j + rhs.j, lhs.k + rhs.k)

  def -(rhs: A): Quaternion[A] =
    Quaternion(r - rhs, i, j, k)
  def -(rhs: Complex[A]): Quaternion[A] =
    Quaternion(r - rhs.real, i - rhs.imag, j, k)
  def -(rhs: Quaternion[A]): Quaternion[A] =
    Quaternion(lhs.r - rhs.r, lhs.i - rhs.i, lhs.j - rhs.j, lhs.k - rhs.k)

  def *(rhs: A): Quaternion[A] =
    Quaternion(r * rhs, i * rhs, j * rhs, k * rhs)
  def *(rhs: Complex[A]): Quaternion[A] =
    Quaternion(
      (r * rhs.real) + (i * rhs.imag),
      (r * rhs.imag) + (i * rhs.real),
      (j * rhs.real) + (k * rhs.imag),
      (j * rhs.imag) + (k * rhs.real)
    )
  def *(rhs: Quaternion[A]): Quaternion[A] = Quaternion(
    (lhs.r * rhs.r) - (lhs.i * rhs.i) - (lhs.j * rhs.j) - (lhs.k * rhs.k),
    (lhs.r * rhs.i) + (lhs.i * rhs.r) + (lhs.j * rhs.k) - (lhs.k * rhs.j),
    (lhs.r * rhs.j) - (lhs.i * rhs.k) + (lhs.j * rhs.r) + (lhs.k * rhs.i),
    (lhs.r * rhs.k) + (lhs.i * rhs.j) - (lhs.j * rhs.i) + (lhs.k * rhs.r)
  )

  def /(rhs: A): Quaternion[A] =
    Quaternion(r / rhs, i / rhs, j / rhs, k / rhs)
  def /(rhs: Complex[A]): Quaternion[A] =
    lhs * Quaternion(rhs).reciprocal
  def /(rhs: Quaternion[A]): Quaternion[A] =
    lhs * rhs.reciprocal

  def pow(k: Int): Quaternion[A] = {
    @tailrec def loop(p: Quaternion[A], b: Quaternion[A], e: Int): Quaternion[A] =
      if (e == 0) p
      else if ((e & 1) == 1) loop(p * b, b * b, e >>> 1)
      else loop(p, b * b, e >>> 1)

    if (k >= 0) loop(Quaternion.one, this, k) else sys.error(s"illegal exponent: $k")
  }
  def **(k: Int): Quaternion[A] = pow(k)

  def fpow(k0: A): Quaternion[A] =
    if (k0.signum < 0) {
      Quaternion.zero
    } else if (k0 == zero) {
      Quaternion.one
    } else if (k0 == one) {
      this
    } else if (!isReal) {
      val s = (i ** 2 + j ** 2 + k ** 2).sqrt
      val v = Quaternion(zero, i / s, j / s, k / s)
      val n = norm
      val t = acos(r / n)
      (Quaternion(cos(t * k0)) + v * sin(t * k0)) * n.fpow(k0)
    } else if (r.signum >= 0) {
      Quaternion(r.fpow(k0))
    } else {
      Quaternion(Complex(r).pow(Complex(k0)))
    }

  def floor: Quaternion[A] = Quaternion(r.floor, i.floor, j.floor, k.floor)
  def ceil: Quaternion[A] = Quaternion(r.ceil, i.ceil, j.ceil, k.ceil)
  def round: Quaternion[A] = Quaternion(r.round, i.round, j.round, k.round)

  def /~(rhs: A): Quaternion[A] =
    (lhs / rhs).floor
  def /~(rhs: Complex[A]): Quaternion[A] =
    (lhs / rhs).floor
  def /~(rhs: Quaternion[A]): Quaternion[A] =
    (lhs / rhs).floor

  def %(rhs: A): Quaternion[A] =
    lhs - (lhs /~ rhs)
  def %(rhs: Complex[A]): Quaternion[A] =
    lhs - (lhs /~ rhs)
  def %(rhs: Quaternion[A]): Quaternion[A] =
    lhs - (lhs /~ rhs)

  def /%(rhs: A): (Quaternion[A], Quaternion[A]) = {
    val q = lhs /~ rhs
    (q, lhs - q)
  }
  def /%(rhs: Complex[A]): (Quaternion[A], Quaternion[A]) = {
    val q = lhs /~ rhs
    (q, lhs - q)
  }
  def /%(rhs: Quaternion[A]): (Quaternion[A], Quaternion[A]) = {
    val q = lhs /~ rhs
    (q, lhs - q)
  }

  def dot(rhs: Quaternion[A]): A =
    (lhs.conjugate * rhs + rhs.conjugate * lhs).r * double(0.5)
}
