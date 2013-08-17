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
  val i = Quaternion(0.0, 1.0, 0.0, 0.0)
  val j = Quaternion(0.0, 0.0, 1.0, 0.0)
  val k = Quaternion(0.0, 0.0, 0.0, 1.0)

  val zero = Quaternion(0.0, 0.0, 0.0, 0.0)
  val one = Quaternion(1.0, 0.0, 0.0, 0.0)

  def apply(n: Double): Quaternion = Quaternion(n, 0.0, 0.0, 0.0)
}

final case class Quaternion(r: Double, i: Double, j: Double, k: Double)
    extends ScalaNumber with ScalaNumericConversions with Serializable { lhs =>
  
  def doubleValue: Double = if (isReal) r.toDouble else Double.NaN
  def floatValue: Float = if (isReal) r.toFloat else Float.NaN
  def longValue: Long = r.toLong
  def intValue: Int = r.toInt

  def isReal: Boolean = i == 0.0 && j == 0.0 && k == 0.0
  def isWhole: Boolean = isReal && r.isWhole

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
    if (!isReal) {
      sys.error("fixme")
    } else if (((k0 & 1) == 1) ^ (r >= 0)) {
      Quaternion(r.abs.nroot(k0))
    } else {
      Quaternion(0.0, r.abs.nroot(k0), 0.0, 0.0)
    }

  def unit(): Quaternion = {
    val n = norm
    Quaternion((r ** 2) / n, (i ** 2) / n, (j ** 2) / n, (k ** 2) / n)
  }

  def star(): Quaternion =
    (this +
      Quaternion.i * this * Quaternion.i +
      Quaternion.j * this * Quaternion.j +
      Quaternion.k * this * Quaternion.k) * -0.5

  def abv(): (Double, Double, (Double, Double, Double)) = {
    val a = r
    val b = (i ** 2 + j ** 2 + k ** 2).sqrt
    Complex(a, b).sqrt
    (a, b, (i / b, j / b, k / b))
  }

  def +(rhs: Double): Quaternion =
    Quaternion(r + rhs, i, j, k)

  def +(rhs: Quaternion): Quaternion =
    Quaternion(lhs.r + rhs.r, lhs.i + rhs.i, lhs.j + rhs.j, lhs.k + rhs.k)

  def -(rhs: Double): Quaternion =
    Quaternion(r - rhs, i, j, k)

  def -(rhs: Quaternion): Quaternion =
    Quaternion(lhs.r - rhs.r, lhs.i - rhs.i, lhs.j - rhs.j, lhs.k - rhs.k)

  def *(rhs: Double): Quaternion =
    Quaternion(r * rhs, i * rhs, j * rhs, k * rhs)

  def *(rhs: Quaternion): Quaternion = Quaternion(
    (lhs.r * rhs.r) - (lhs.i * rhs.i) - (lhs.j * rhs.j) - (lhs.k * rhs.k),
    (lhs.r * rhs.i) + (lhs.i * rhs.r) + (lhs.j * rhs.k) - (lhs.k * rhs.j),
    (lhs.r * rhs.j) - (lhs.i * rhs.k) + (lhs.j * rhs.r) + (lhs.k * rhs.i),
    (lhs.r * rhs.k) + (lhs.i * rhs.j) - (lhs.j * rhs.i) + (lhs.k * rhs.r)
  )

  def /(rhs: Double): Quaternion =
    Quaternion(r / rhs, i / rhs, j / rhs, k / rhs)

  def /(rhs: Quaternion): Quaternion =
    lhs * rhs.reciprocal

  def pow(k: Int) = {
    @tailrec def loop(p: Quaternion, b: Quaternion, e: Int): Quaternion =
      if (e == 0) p
      else if ((e & 1) == 1) loop(p * b, b * b, e >>> 1)
      else loop(p, b * b, e >>> 1)

    if (k >= 0) loop(Quaternion.one, this, k) else sys.error("illegal exponent")
  }
  def **(k: Int) = pow(k)
}
