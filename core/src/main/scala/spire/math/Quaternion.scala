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

import scala.math.{ScalaNumber, ScalaNumericConversions}
import spire.algebra._
import spire.syntax.field._
import spire.syntax.isReal._
import spire.syntax.nroot._

object Quaternion extends QuaternionInstances {
  def i[@sp(Float, Double) A](implicit f: CRing[A]): Quaternion[A] =
    Quaternion(f.zero, f.one, f.zero, f.zero)
  def j[@sp(Float, Double) A](implicit f: CRing[A]): Quaternion[A] =
    Quaternion(f.zero, f.zero, f.one, f.zero)
  def k[@sp(Float, Double) A](implicit f: CRing[A]): Quaternion[A] =
    Quaternion(f.zero, f.zero, f.zero, f.one)

  def zero[@sp(Float, Double) A](implicit f: CRing[A]): Quaternion[A] =
    Quaternion(f.zero, f.zero, f.zero, f.zero)
  def one[@sp(Float, Double) A](implicit f: CRing[A]): Quaternion[A] =
    Quaternion(f.one, f.zero, f.zero, f.zero)

  def apply[@sp(Float, Double) A](a: A)(implicit f: CRing[A]): Quaternion[A] =
    Quaternion(a, f.zero, f.zero, f.zero)
  def apply[@sp(Float, Double) A](c: Complex[A])(implicit f: CRing[A]): Quaternion[A] =
    Quaternion(c.real, c.imag, f.zero, f.zero)
}

/**
 * Quaternion algebra over an ordered field.
 */
private[math] trait QuaternionOverField[A]
    extends Eq[Quaternion[A]]
    with DivisionRing[Quaternion[A]]
    with FieldAssociativeAlgebra[Quaternion[A], A]
    with Involution[Quaternion[A]] {

  implicit def o: Order[A]
  implicit def s: Signed[A]

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

  def timesl(a: A, q: Quaternion[A]): Quaternion[A] = q * a
  def dot(x: Quaternion[A], y: Quaternion[A]): A = x.dot(y)

  def adjoint(a: Quaternion[A]): Quaternion[A] = a.conjugate
}

private[math] trait QuaternionOverRichField[A]
    extends QuaternionOverField[A]
    with NRoot[Quaternion[A]]
    with InnerProductSpace[Quaternion[A], A] {

  implicit def n: NRoot[A]
  implicit def t: Trig[A]

  def nroot(a: Quaternion[A], k: Int): Quaternion[A] = a.nroot(k)
  override def sqrt(a: Quaternion[A]): Quaternion[A] = a.sqrt
  def fpow(a: Quaternion[A], b: Quaternion[A]): Quaternion[A] = a.fpow(b.r) // FIXME

}

trait QuaternionInstances1 {

  implicit def QuaternionOverField[A](implicit f0: Field[A], o0: Order[A], s0: Signed[A]): QuaternionOverField[A] =
    new QuaternionOverField[A] {
      val scalar = f0
      val o = o0
      val s = s0
    }

}

trait QuaternionInstances extends QuaternionInstances1 {

  implicit def QuaternionOverRichField[A](implicit
    f0: Field[A],
    n0: NRoot[A],
    o0: Order[A],
    s0: Signed[A],
    t0: Trig[A]
  ): QuaternionOverRichField[A] =
    new QuaternionOverRichField[A] {
      val scalar = f0
      val n = n0
      val o = o0
      val s = s0
      val t = t0
    }

}

/**
 * Quaternions defined over a subset A of the real numbers.
 */
final case class Quaternion[@sp(Float, Double) A](r: A, i: A, j: A, k: A)
    extends ScalaNumber
    with ScalaNumericConversions
    with Serializable { lhs =>

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

  final override def isValidInt: Boolean =
    sillyIsReal && anyIsValidInt(r)

  // important to keep in sync with Complex[_]
  override def hashCode: Int =
    if (sillyIsReal) r.##
    else (19 * r.##) + (41 * i.##) + (13 * j.##) + (77 * k.##) + 97

  // not typesafe, so this is the best we can do :(
  override def equals(that: Any): Boolean = that match {
    case that: Quaternion[?] => this === that
    case that: Complex[?] =>
      r == that.real && i == that.imag && anyIsZero(j) && anyIsZero(k)
    case that =>
      sillyIsReal && r == that
  }

  def ===(that: Quaternion[?]): Boolean =
    r == that.r && i == that.i && j == that.j && k == that.k

  def =!=(that: Quaternion[?]): Boolean =
    !(this === that)

  def isZero(implicit s: Signed[A]): Boolean = r.isSignZero && i.isSignZero && j.isSignZero && k.isSignZero
  def isReal(implicit s: Signed[A]): Boolean = i.isSignZero && j.isSignZero && k.isSignZero
  def isPure(implicit s: Signed[A]): Boolean = r.isSignZero

  def real(implicit s: CRing[A]): Quaternion[A] = Quaternion(r)
  def pure(implicit s: CRing[A]): Quaternion[A] = Quaternion(s.zero, i, j, k)

  def abs(implicit f: Field[A], n: NRoot[A]): A =
    (r.pow(2) + i.pow(2) + j.pow(2) + k.pow(2)).sqrt

  def pureAbs(implicit f: Field[A], n: NRoot[A]): A =
    (i.pow(2) + j.pow(2) + k.pow(2)).sqrt

  def eqv(rhs: Quaternion[A])(implicit o: Eq[A]): Boolean =
    lhs.r === rhs.r && lhs.i === rhs.i && lhs.j === rhs.j && lhs.k === rhs.k

  def neqv(rhs: Quaternion[A])(implicit o: Eq[A]): Boolean =
    lhs.r =!= rhs.r && lhs.i =!= rhs.i && lhs.j =!= rhs.j && lhs.k =!= rhs.k

  override def toString: String = s"($r + ${i}i + ${j}j + ${k}k)"

  def toComplex: Complex[A] = Complex(r, i)

  def signum(implicit s: Signed[A]): Int = r.signum match {
    case 0 =>
      i.signum match {
        case 0 =>
          j.signum match {
            case 0 => k.signum
            case n => n
          }
        case n => n
      }
    case n => n
  }

  def quaternionSignum(implicit f: Field[A], n: NRoot[A], s: Signed[A]): Quaternion[A] =
    if (isZero) this else this / abs

  def pureSignum(implicit f: Field[A], n: NRoot[A], s: Signed[A]): Quaternion[A] =
    if (isReal) Quaternion.zero[A] else pure / pureAbs

  def unary_-(implicit s: CRing[A]): Quaternion[A] =
    Quaternion(-r, -i, -j, -k)

  def conjugate(implicit s: CRing[A]): Quaternion[A] =
    Quaternion(r, -i, -j, -k)

  def reciprocal(implicit f: Field[A]): Quaternion[A] =
    conjugate / (r.pow(2) + i.pow(2) + j.pow(2) + k.pow(2))

  def sqrt(implicit f: Field[A], nr: NRoot[A], s: Signed[A]): Quaternion[A] =
    if (!isReal) {
      val n = (r + abs).sqrt
      Quaternion(n, i / n, j / n, k / n) / f.fromInt(2).sqrt
    } else if (r.signum >= 0) {
      Quaternion(r.sqrt)
    } else {
      Quaternion(f.zero, r.abs.sqrt, f.zero, f.zero)
    }

  def nroot(m: Int)(implicit f: Field[A], nr: NRoot[A], or: Order[A], si: Signed[A], tr: Trig[A]): Quaternion[A] =
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

  def unit(implicit f: Field[A], n: NRoot[A]): Quaternion[A] =
    Quaternion(r.pow(2), i.pow(2), j.pow(2), k.pow(2)) / abs

  def +(rhs: A)(implicit s: CRing[A]): Quaternion[A] =
    Quaternion(r + rhs, i, j, k)
  def +(rhs: Complex[A])(implicit s: CRing[A]): Quaternion[A] =
    Quaternion(r + rhs.real, i + rhs.imag, j, k)
  def +(rhs: Quaternion[A])(implicit s: CRing[A]): Quaternion[A] =
    Quaternion(lhs.r + rhs.r, lhs.i + rhs.i, lhs.j + rhs.j, lhs.k + rhs.k)

  def -(rhs: A)(implicit s: CRing[A]): Quaternion[A] =
    Quaternion(r - rhs, i, j, k)
  def -(rhs: Complex[A])(implicit s: CRing[A]): Quaternion[A] =
    Quaternion(r - rhs.real, i - rhs.imag, j, k)
  def -(rhs: Quaternion[A])(implicit s: CRing[A]): Quaternion[A] =
    Quaternion(lhs.r - rhs.r, lhs.i - rhs.i, lhs.j - rhs.j, lhs.k - rhs.k)

  def *(rhs: A)(implicit s: CRing[A]): Quaternion[A] =
    Quaternion(r * rhs, i * rhs, j * rhs, k * rhs)
  def *(rhs: Complex[A])(implicit s: CRing[A]): Quaternion[A] =
    Quaternion(
      (r * rhs.real) - (i * rhs.imag),
      (r * rhs.imag) + (i * rhs.real),
      (j * rhs.real) + (k * rhs.imag),
      (j * rhs.imag) + (k * rhs.real)
    )
  def *(rhs: Quaternion[A])(implicit s: CRing[A]): Quaternion[A] = Quaternion(
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

  def pow(k: Int)(implicit s: CRing[A]): Quaternion[A] = {
    @tailrec def loop(p: Quaternion[A], b: Quaternion[A], e: Int): Quaternion[A] =
      if (e == 0) p
      else if ((e & 1) == 1) loop(p * b, b * b, e >>> 1)
      else loop(p, b * b, e >>> 1)

    if (k >= 0) loop(Quaternion.one[A], this, k)
    else throw new IllegalArgumentException(s"illegal exponent: $k")
  }

  def **(k: Int)(implicit s: CRing[A]): Quaternion[A] = pow(k)

  def fpow(k0: A)(implicit f: Field[A], nr: NRoot[A], or: Order[A], si: Signed[A], tr: Trig[A]): Quaternion[A] =
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

  def dot(rhs: Quaternion[A])(implicit f: Field[A]): A =
    (lhs.conjugate * rhs + rhs.conjugate * lhs).r / f.fromInt(2)

}
