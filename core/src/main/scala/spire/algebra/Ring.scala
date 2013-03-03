package spire.algebra

import annotation.tailrec
import scala.{specialized => spec}

import java.lang.Math

import spire.math._
import spire.macrosk.Ops


/**
 * Ring represents a set (A) that is a group over addition (+) and a monoid
 * over multiplication (*). Aside from this, the multiplication must distribute
 * over addition.
 *
 * Ring implements some methods (for example fromInt) in terms of other more
 * fundamental methods (zero, one and plus). Where possible, these methods
 * should be overridden by more efficient implementations.
 */
trait Ring[@spec(Int,Long,Float,Double) A] extends Semiring[A] with Rig[A] with Rng[A] with AdditiveAbGroup[A] {
  def fromInt(n: Int): A =
    if (n < 0) _fromInt(negate(one), -n, zero)
    else _fromInt(one, n, zero)
  
  @tailrec private def _fromInt(a:A, n:Int, sofar:A):A =
    if (n == 0) sofar
    else if (n % 2 == 1) _fromInt(plus(a, a), n / 2, plus(sofar, a))
    else _fromInt(plus(a, a), n / 2, sofar)
}

object Ring {
  implicit object RationalIsRing extends RationalIsRing
  implicit object RealIsRing extends RealIsRing
  implicit object SafeLongIsRing extends SafeLongIsRing
  implicit object NumberIsRing extends NumberIsRing

  implicit def complexIsRing[A: Fractional: Trig] = new ComplexIsRing[A] {
    val f = Fractional[A]
    val t = Trig[A]
  }

  implicit def gaussianIsRing[A: Integral] = new GaussianIsRing[A] {
    val f = Integral[A]
  }

  @inline final def apply[A](implicit r:Ring[A]):Ring[A] = r
}

trait RationalIsRing extends Ring[Rational] {
  override def minus(a:Rational, b:Rational): Rational = a - b
  def negate(a:Rational): Rational = -a
  def one: Rational = Rational.one
  def plus(a:Rational, b:Rational): Rational = a + b
  override def pow(a:Rational, b:Int): Rational = a.pow(b)
  override def times(a:Rational, b:Rational): Rational = a * b
  def zero: Rational = Rational.zero
  
  override def fromInt(n: Int): Rational = Rational(n)
}

trait RealIsRing extends Ring[Real] {
  override def minus(a: Real, b: Real): Real = a - b
  def negate(a: Real): Real = -a
  def one: Real = Real(1)
  def plus(a: Real, b: Real): Real = a + b
  override def pow(a: Real, b: Int): Real = a pow b
  override def times(a: Real, b: Real): Real = a * b
  def zero: Real = Real(0)
  
  override def fromInt(n: Int): Real = Real(n)
}

trait SafeLongIsRing extends Ring[SafeLong] {
  override def minus(a:SafeLong, b:SafeLong): SafeLong = a - b
  def negate(a:SafeLong): SafeLong = -a
  val one: SafeLong = SafeLong(1)
  def plus(a:SafeLong, b:SafeLong): SafeLong = a + b
  override def pow(a:SafeLong, b:Int): SafeLong = a pow b
  override def times(a:SafeLong, b:SafeLong): SafeLong = a * b
  val zero: SafeLong = SafeLong(0)
  
  override def fromInt(n: Int): SafeLong = SafeLong(n)
}

trait ComplexIsRing[@spec(Float, Double) A] extends Ring[Complex[A]] {
  implicit def f:Fractional[A]
  implicit def t:Trig[A]

  override def minus(a:Complex[A], b:Complex[A]): Complex[A] = a - b
  def negate(a:Complex[A]): Complex[A] = -a
  def one: Complex[A] = Complex.one(f, t)
  def plus(a:Complex[A], b:Complex[A]): Complex[A] = a + b
  override def pow(a:Complex[A], b:Int):Complex[A] = a.pow(b)
  override def times(a:Complex[A], b:Complex[A]): Complex[A] = a * b
  def zero: Complex[A] = Complex.zero(f, t)

  override def fromInt(n: Int): Complex[A] = Complex.fromInt[A](n)
}

trait GaussianIsRing[@spec(Int, Long) A] extends Ring[Gaussian[A]] {
  implicit def f:Integral[A]

  override def minus(a:Gaussian[A], b:Gaussian[A]): Gaussian[A] = a - b
  def negate(a:Gaussian[A]): Gaussian[A] = -a
  def one: Gaussian[A] = Gaussian.one[A]
  def plus(a:Gaussian[A], b:Gaussian[A]): Gaussian[A] = a + b
  override def pow(a:Gaussian[A], b:Int):Gaussian[A] = a.pow(b)
  override def times(a:Gaussian[A], b:Gaussian[A]): Gaussian[A] = a * b
  def zero: Gaussian[A] = Gaussian.zero[A]

  override def fromInt(n: Int): Gaussian[A] = Gaussian.fromInt[A](n)
}

trait NumberIsRing extends Ring[Number] {
  override def minus(a:Number, b:Number): Number = a - b
  def negate(a:Number): Number = -a
  val one: Number = Number.one
  def plus(a:Number, b:Number): Number = a + b
  override def pow(a:Number, b:Int): Number = a.pow(Number(b))
  override def times(a:Number, b:Number): Number = a * b
  val zero: Number = Number.zero
  
  override def fromInt(n: Int): Number = Number(n)
}
