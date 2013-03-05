package spire.math

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.math.{ScalaNumber, ScalaNumericConversions}

import java.lang.Math

import spire.algebra.{ Order, EuclideanRing }

object EuclideanRational {
  def one[@spec(Int, Long) A](implicit f: EuclideanRing[A]): EuclideanRational[A] =
    EuclideanRational(f.one, f.one)
  def zero[@spec(Int, Long) A](implicit f: EuclideanRing[A]): EuclideanRational[A] =
    EuclideanRational(f.zero, f.one)
  def apply[@spec(Int, Long) A](a: A)(implicit f: EuclideanRing[A]): EuclideanRational[A] =
    EuclideanRational(a, f.one)
}

case class EuclideanRational[@spec(Int, Long) A](n: A, d: A)(implicit f: EuclideanRing[A]) {
  lhs =>

  override def toString(): String = "%s/%s" format (n, d)

  def reciprocal(): EuclideanRational[A] = EuclideanRational(d, n)

  def quantize(): A = f.quot(n, d)

  def ceil()(implicit o: Order[A]): A = if (o.compare(f.mod(n, d), f.zero) > 0)
    f.plus(f.quot(n, d), f.one)
  else
    f.quot(n, d)

  def floor()(implicit o:Order[A]): A = if (o.compare(f.mod(n, d), f.zero) < 0)
    f.minus(f.quot(n, d), f.one)
  else
    f.quot(n, d)

  def unary_-(): EuclideanRational[A] = EuclideanRational(f.negate(n), d)

  def +(rhs: EuclideanRational[A]): EuclideanRational[A] = {
    val dgcd = f.gcd(lhs.d, rhs.d)
    val ld = f.quot(lhs.d, dgcd)
    val rd = f.quot(rhs.d, dgcd)
    val num = f.plus(f.times(lhs.n, rd), f.times(rhs.n, ld))
    val ngcd = f.gcd(num, dgcd)
    EuclideanRational(f.quot(num, ngcd), f.times(ld, f.quot(rhs.d, ngcd)))
  }

  def -(rhs: EuclideanRational[A]): EuclideanRational[A] = lhs + (-rhs)

  def *(rhs: EuclideanRational[A]): EuclideanRational[A] = {
    val a = f.gcd(lhs.n, rhs.d)
    val b = f.gcd(lhs.d, rhs.n)
    val nn = f.times(f.quot(lhs.n, a), f.quot(rhs.n, b))
    val dd = f.times(f.quot(lhs.d, b), f.quot(rhs.d, a))
    EuclideanRational(nn, dd)
  }

  def /(rhs: EuclideanRational[A]): EuclideanRational[A] =
    lhs * rhs.reciprocal

  def /~(rhs: EuclideanRational[A]): EuclideanRational[A] =
    EuclideanRational((lhs / rhs).quantize, f.one)

  def %(rhs: EuclideanRational[A]): EuclideanRational[A] =
    lhs - (lhs /~ rhs) * rhs

  def /%(rhs: EuclideanRational[A]): (EuclideanRational[A], EuclideanRational[A]) = {
    val q = lhs /~ rhs
    (q, lhs - q * rhs)
  }

  def **(rhs: Int): EuclideanRational[A] =
    EuclideanRational(f.pow(n, rhs), f.pow(d, rhs))
}
