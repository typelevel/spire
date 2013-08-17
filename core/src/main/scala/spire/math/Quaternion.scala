package spire.math

import spire.algebra._
import spire.std.float._
import spire.std.double._
import spire.std.bigDecimal._

import spire.syntax.convertableFrom._
import spire.syntax.field._
import spire.syntax.isReal._
import spire.syntax.nroot._
import spire.syntax.order._

import scala.{specialized => spec}
import scala.annotation.tailrec
import scala.math.{ScalaNumber, ScalaNumericConversions}
import java.lang.Math

object Quaternion {
  def i = Quaternion(0.0, 1.0, 0.0, 0.0)
  def j = Quaternion(0.0, 0.0, 1.0, 0.0)
  def k = Quaternion(0.0, 0.0, 0.0, 1.0)

  def zero = Quaternion(0.0, 0.0, 0.0, 0.0)
  def one = Quaternion(1.0, 0.0, 0.0, 0.0)

  def apply(n: Double): Quaternion =
    Quaternion(n, 0.0, 0.0, 0.0)
  def apply(c: Complex[Double]): Quaternion =
    Quaternion(c.real, c.imag, 0.0, 0.0)
}

final case class Quaternion(r: Double, i: Double, j: Double, k: Double)
    extends ScalaNumber with ScalaNumericConversions with Serializable { lhs =>
  
  def doubleValue: Double = if (isReal) r.toDouble else Double.NaN
  def floatValue: Float = if (isReal) r.toFloat else Float.NaN
  def longValue: Long = r.toLong
  def intValue: Int = r.toInt

  def isReal: Boolean = i == 0.0 && j == 0.0 && k == 0.0
  def isPure: Boolean = r == 0.0
  def isWhole: Boolean = isReal && r.isWhole

  def real: Quaternion = Quaternion(r, 0.0, 0.0, 0.0)
  def unreal: Quaternion = Quaternion(0.0, i, j, k)

  override def isValidInt: Boolean =
    i == 0.0 && j == 0.0 && k == 0.0 && r.isWhole &&
    r <= Int.MaxValue && r >= Int.MinValue

  def underlying = (r, i, j, k)

  // important to keep in sync with Complex[_]
  override def hashCode: Int =
    if (isValidInt) r.toInt
    else 19 * r.## + 41 * i.## + 13 * j.## + 77 * k.## + 97

  override def equals(that: Any): Boolean = that match {
    case that: Quaternion =>
      r == that.r && i == that.i && j == that.j && k == that.k
    case that: Complex[_] =>
      r == that.real && i == that.imag && j == 0.0 && k == 0.0
    case that =>
      unifiedPrimitiveEquals(that)
  }

  override def toString: String = s"($r + ${i}i + ${j}j + ${k}k)"

  def unary_-(): Quaternion =
    Quaternion(-r, -i, -j, -k)

  def conjugate(): Quaternion =
    Quaternion(r, -i, -j, -k)

  def norm(): Double =
    (r ** 2 + i ** 2 + j ** 2 + k ** 2).sqrt

  def reciprocal(): Quaternion =
    conjugate / (r ** 2 + i ** 2 + j ** 2 + k ** 2)

  def sqrt(): Quaternion =
    if (!isReal) {
      val n = (r + norm).sqrt
      Quaternion(n, i / n, j / n, k / n) / 2.0.sqrt
    } else if (r >= 0) {
      Quaternion(r.sqrt)
    } else {
      Quaternion(0.0, r.abs.sqrt, 0.0, 0.0)
    }

  def nroot(k0: Int): Quaternion =
    if (k0 <= 0) {
      sys.error(s"illegal root: $k0")
    } else if (k0 == 1) {
      this
    } else if (!isReal) {
      val s = (i ** 2 + j ** 2 + k ** 2).sqrt
      val v = Quaternion(0.0, i / s, j / s, k / s)
      val n = norm
      val t = spire.math.acos(r / n)
      (Quaternion(cos(t / k0)) + v * sin(t / k0)) * n.nroot(k0)
    } else if (((k0 & 1) == 1) ^ (r >= 0)) {
      Quaternion(r.abs.nroot(k0))
    } else {
      Quaternion(0.0, r.abs.nroot(k0), 0.0, 0.0)
    }

  def unit(): Quaternion = {
    val n = norm
    Quaternion((r ** 2) / n, (i ** 2) / n, (j ** 2) / n, (k ** 2) / n)
  }

  def +(rhs: Double): Quaternion =
    Quaternion(r + rhs, i, j, k)
  def +(rhs: Complex[Double]): Quaternion =
    Quaternion(r + rhs.real, i + rhs.imag, j, k)
  def +(rhs: Quaternion): Quaternion =
    Quaternion(lhs.r + rhs.r, lhs.i + rhs.i, lhs.j + rhs.j, lhs.k + rhs.k)

  def -(rhs: Double): Quaternion =
    Quaternion(r - rhs, i, j, k)
  def -(rhs: Complex[Double]): Quaternion =
    Quaternion(r - rhs.real, i - rhs.imag, j, k)
  def -(rhs: Quaternion): Quaternion =
    Quaternion(lhs.r - rhs.r, lhs.i - rhs.i, lhs.j - rhs.j, lhs.k - rhs.k)

  def *(rhs: Double): Quaternion =
    Quaternion(r * rhs, i * rhs, j * rhs, k * rhs)
  def *(rhs: Complex[Double]): Quaternion =
    Quaternion(
      (r * rhs.real) + (i * rhs.imag),
      (r * rhs.imag) + (i * rhs.real),
      (j * rhs.real) + (k * rhs.imag),
      (j * rhs.imag) + (k * rhs.real)
    )
  def *(rhs: Quaternion): Quaternion = Quaternion(
    (lhs.r * rhs.r) - (lhs.i * rhs.i) - (lhs.j * rhs.j) - (lhs.k * rhs.k),
    (lhs.r * rhs.i) + (lhs.i * rhs.r) + (lhs.j * rhs.k) - (lhs.k * rhs.j),
    (lhs.r * rhs.j) - (lhs.i * rhs.k) + (lhs.j * rhs.r) + (lhs.k * rhs.i),
    (lhs.r * rhs.k) + (lhs.i * rhs.j) - (lhs.j * rhs.i) + (lhs.k * rhs.r)
  )

  def /(rhs: Double): Quaternion =
    Quaternion(r / rhs, i / rhs, j / rhs, k / rhs)
  def /(rhs: Complex[Double]): Quaternion =
    lhs * Quaternion(rhs).reciprocal
  def /(rhs: Quaternion): Quaternion =
    lhs * rhs.reciprocal

  def pow(k: Int): Quaternion = {
    @tailrec def loop(p: Quaternion, b: Quaternion, e: Int): Quaternion =
      if (e == 0) p
      else if ((e & 1) == 1) loop(p * b, b * b, e >>> 1)
      else loop(p, b * b, e >>> 1)

    if (k >= 0) loop(Quaternion.one, this, k) else sys.error(s"illegal exponent: $k")
  }
  def **(k: Int): Quaternion = pow(k)

  def fpow(k0: Double): Quaternion =
    if (k0 < 0.0) {
      Quaternion.zero
    } else if (k0 == 0.0) {
      Quaternion.one
    } else if (k0 == 1.0) {
      this
    } else if (!isReal) {
      val s = (i ** 2 + j ** 2 + k ** 2).sqrt
      val v = Quaternion(0.0, i / s, j / s, k / s)
      val n = norm
      val t = acos(r / n)
      (Quaternion(cos(t * k0)) + v * sin(t * k0)) * n.fpow(k0)
    } else if (r >= 0) {
      Quaternion(r.fpow(k0))
    } else {
      Quaternion(Complex(r).pow(Complex(k0)))
    }

  def floor: Quaternion = Quaternion(r.floor, i.floor, j.floor, k.floor)
  def ceil: Quaternion = Quaternion(r.ceil, i.ceil, j.ceil, k.ceil)
  def round: Quaternion = Quaternion(r.round, i.round, j.round, k.round)

  def /~(rhs: Double): Quaternion =
    (lhs / rhs).floor
  def /~(rhs: Complex[Double]): Quaternion =
    (lhs / rhs).floor
  def /~(rhs: Quaternion): Quaternion =
    (lhs / rhs).floor

  def %(rhs: Double): Quaternion =
    lhs - (lhs /~ rhs)
  def %(rhs: Complex[Double]): Quaternion =
    lhs - (lhs /~ rhs)
  def %(rhs: Quaternion): Quaternion =
    lhs - (lhs /~ rhs)

  def /%(rhs: Double): (Quaternion, Quaternion) = {
    val q = lhs /~ rhs
    (q, lhs - q)
  }
  def /%(rhs: Complex[Double]): (Quaternion, Quaternion) = {
    val q = lhs /~ rhs
    (q, lhs - q)
  }
  def /%(rhs: Quaternion): (Quaternion, Quaternion) = {
    val q = lhs /~ rhs
    (q, lhs - q)
  }

  def dot(rhs: Quaternion): Double =
    (lhs.conjugate * rhs + rhs.conjugate * lhs) * 0.5
}
