package spire.algebra

import spire.math._
import spire.macrosk.Ops

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.math.{abs, ceil, floor}

import spire.math.{ConvertableTo, ConvertableFrom, Number}

trait EuclideanRing[@spec(Int,Long,Float,Double) A] extends Ring[A] {
  def quot(a:A, b:A):A
  def mod(a:A, b:A):A
  def quotmod(a:A, b:A): (A, A) = (quot(a, b), mod(a, b))

  //def gcd(a: A, b: A)(implicit eq: Eq[A]): A = euclid(a, b)
  def gcd(a: A, b: A): A

  //def lcm(a: A, b: A)(implicit eq: Eq[A]): A = times(quot(a, gcd(a, b)), b)

  @tailrec protected[this] final def euclid(a:A, b:A)(implicit eq: Eq[A]):A =
    if (eq.eqv(b, zero)) a else euclid(b, mod(a, b))
}

final class EuclideanRingOps[A](lhs:A)(implicit ev:EuclideanRing[A]) {
  def /~(rhs:A) = macro Ops.binop[A, A]
  def %(rhs:A) = macro Ops.binop[A, A]
  def /%(rhs:A) = macro Ops.binop[A, A]

  // TODO: This is a bit
  def /~(rhs:Int): A = macro Ops.binopWithSelfLift[Int, Ring[A], A]
  def %(rhs:Int): A = macro Ops.binopWithSelfLift[Int, Ring[A], A]
  def /%(rhs:Int): (A, A) = macro Ops.binopWithSelfLift[Int, Ring[A], (A, A)]

  def /~(rhs:Double)(implicit ev1:ConvertableTo[A]): A = macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def %(rhs:Double)(implicit ev1:ConvertableTo[A]): A = macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def /%(rhs:Double)(implicit ev1:ConvertableTo[A]): (A, A) = macro Ops.binopWithLift[Double, ConvertableTo[A], (A, A)]

  def /~(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) /~ rhs
  def %(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) % rhs
  def /%(rhs:Number)(implicit c:ConvertableFrom[A]): (Number, Number) = c.toNumber(lhs) /% rhs
}

object EuclideanRing {
  implicit object RationalIsEuclideanRing extends RationalIsEuclideanRing
  implicit object RealIsEuclideanRing extends RealIsEuclideanRing
  implicit object SafeLongIsEuclideanRing extends SafeLongIsEuclideanRing
  implicit object NumberIsEuclideanRing extends NumberIsEuclideanRing

  implicit def complexIsEuclideanRing[A: Fractional: Trig] =
    new ComplexIsEuclideanRing[A] {
      val f = Fractional[A]
      val t = Trig[A]
    }

  implicit def gaussianIsEuclideanRing[A: Integral] =
    new GaussianIsEuclideanRing[A] {
      val f = Integral[A]
    }

  @inline final def apply[A](implicit e:EuclideanRing[A]):EuclideanRing[A] = e
}

trait RationalIsEuclideanRing extends EuclideanRing[Rational] with RationalIsRing {
  def quot(a:Rational, b:Rational) = a /~ b
  def mod(a:Rational, b:Rational) = a % b
  override def quotmod(a:Rational, b:Rational) = a /% b
  def gcd(a:Rational, b:Rational):Rational = _gcd(a.abs, b.abs)
  @tailrec private def _gcd(a:Rational, b:Rational):Rational = {
    if (a.compareToOne < 0) {
      Rational.one
    } else if (b.signum == 0) {
      a
    } else if (b.compareToOne < 0) {
      Rational.one
    } else {
      _gcd(b, a % b)
    }
  }
}

trait RealIsEuclideanRing extends EuclideanRing[Real] with RealIsRing {
  def quot(a: Real, b: Real): Real = a /~ b
  def mod(a: Real, b: Real): Real = a % b
  def gcd(a: Real, b: Real): Real = euclid(a, b)(Eq[Real])
}

trait SafeLongIsEuclideanRing extends EuclideanRing[SafeLong] with SafeLongIsRing {
  def quot(a:SafeLong, b:SafeLong) = a / b
  def mod(a:SafeLong, b:SafeLong) = a % b
  override def quotmod(a:SafeLong, b:SafeLong) = a /% b
  def gcd(a:SafeLong, b:SafeLong) = a.toBigInt.gcd(b.toBigInt)
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

trait GaussianIsEuclideanRing[@spec(Int, Long) A]
extends GaussianIsRing[A] with EuclideanRing[Gaussian[A]] {
  def quot(a:Gaussian[A], b:Gaussian[A]) = a / b
  def mod(a:Gaussian[A], b:Gaussian[A]) = a % b
  override def quotmod(a:Gaussian[A], b:Gaussian[A]) = a /% b
  def gcd(a: Gaussian[A], b: Gaussian[A]): Gaussian[A] = euclid(a, b)(Eq[Gaussian[A]])
}

trait NumberIsEuclideanRing extends EuclideanRing[Number] with NumberIsRing {
  def quot(a:Number, b:Number) = a / b
  def mod(a:Number, b:Number) = a % b
  override def quotmod(a:Number, b:Number) = a /% b
  def gcd(a: Number, b: Number): Number = euclid(a, b)(Eq[Number])
}
