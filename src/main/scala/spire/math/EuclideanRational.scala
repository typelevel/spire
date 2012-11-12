package spire.math

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.math.{ScalaNumber, ScalaNumericConversions}

import java.lang.Math

import spire.algebra.EuclideanRing

object EuclideanRational {
  def one[@spec(Int, Long) A](implicit f: EuclideanRing[A]): EuclideanRational[A] =
    EuclideanRational(f.one, f.one)
  def zero[@spec(Int, Long) A](implicit f: EuclideanRing[A]): EuclideanRational[A] =
    EuclideanRational(f.zero, f.one)
}

case class EuclideanRational[@spec(Int, Long) A]
(n: A, d: A)(implicit f: EuclideanRing[A]) {
  lhs =>

  def inverse() = EuclideanRational(d, n)

  def quantize() = f.quot(n, d)

  def ceil()(implicit o:Order[A]) = if (o.compare(f.mod(n, d), f.zero) > 0)
    f.plus(f.quot(n, d), f.one)
  else
    f.quot(n, d)

  def floor()(implicit o:Order[A]) = if (o.compare(f.mod(n, d), f.zero) < 0)
    f.minus(f.quot(n, d), f.one)
  else
    f.quot(n, d)

  def unary_-() = EuclideanRational(f.negate(n), d)

  def +(rhs: EuclideanRational[A]) = {
    val dgcd = f.gcd(lhs.d, rhs.d)
    val ld = f.quot(lhs.d, dgcd)
    val rd = f.quot(rhs.d, dgcd)
    val num = f.plus(f.times(lhs.n, rd), f.times(rhs.n, ld))
    val ngcd = f.gcd(num, dgcd)
    EuclideanRational(
      f.quot(num, ngcd),
      f.times(ld, f.quot(rhs.d, ngcd))
    )
  }

  def -(rhs: EuclideanRational[A]) = lhs + (-rhs)

  def *(rhs: EuclideanRational[A]) = {
    val a = f.gcd(lhs.n, rhs.d)
    val b = f.gcd(lhs.d, rhs.n)
    EuclideanRational(
      f.times(f.quot(lhs.n, a), f.quot(rhs.n, b)),
      f.times(f.quot(lhs.d, b), f.quot(rhs.d, a))
    )
  }

  def /(rhs: EuclideanRational[A]) = lhs * rhs.inverse

  def /~(rhs: EuclideanRational[A]) =
    EuclideanRational((lhs / rhs).quantize, f.one)

  def %(rhs: EuclideanRational[A]) = lhs - (lhs /~ rhs)

  def /%(rhs: EuclideanRational[A]) = {
    val q = lhs /~ rhs
    (q, lhs - q)
  }
}
