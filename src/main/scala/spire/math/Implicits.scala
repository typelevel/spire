package spire.math

import spire.algebra._

import scala.{specialized => spec}

import language.implicitConversions

final class ComplexDoubleOps(lhs:Double) {
  @inline private final def c[A:Fractional:ConvertableTo:Trig]:Complex[A] = {
    Complex(ConvertableFrom[Double].toType[A](lhs), Fractional[A].zero)
  }
  def +[A:Fractional:Trig](rhs:Complex[A]) = c + rhs
  def *[A:Fractional:Trig](rhs:Complex[A]) = c * rhs
  def -[A:Fractional:Trig](rhs:Complex[A]) = c - rhs
  def /[A:Fractional:Trig](rhs:Complex[A]) = c / rhs
  def /~[A:Fractional:Trig](rhs:Complex[A]) = c /~ rhs
  def %[A:Fractional:Trig](rhs:Complex[A]) = c % rhs
  def /%[A:Fractional:Trig](rhs:Complex[A]) = c /% rhs
  def pow[A:Fractional:Trig](rhs:Complex[A]) = c pow rhs
  def **[A:Fractional:Trig](rhs:Complex[A]) = c ** rhs
}

final class LiteralIntOps(lhs:Int) {
  @inline private final def q = Rational(lhs, 1)

  def +(rhs:Rational) = q + rhs
  def -(rhs:Rational) = q - rhs
  def *(rhs:Rational) = q * rhs
  def /(rhs:Rational) = q / rhs

  def /~(rhs:Rational) = q.quot(rhs)
  def %(rhs:Rational) = q % rhs
  def /%(rhs:Rational) = (q.quot(rhs), q % rhs)

  def **(rhs:Rational)(implicit ev:ApproximationContext[Rational]) = q.pow(rhs)

  def <(rhs:Rational) = q.compare(rhs) < 0
  def <=(rhs:Rational) = q.compare(rhs) <= 0
  def >(rhs:Rational) = q.compare(rhs) > 0
  def >=(rhs:Rational) = q.compare(rhs) >= 0

  private def c[A](implicit f:Fractional[A], t:Trig[A]) = Complex(f.fromInt(lhs), f.zero)

  def +[A:Fractional:Trig](rhs:Complex[A]) = c[A] + rhs
  def -[A:Fractional:Trig](rhs:Complex[A]) = c[A] - rhs
  def *[A:Fractional:Trig](rhs:Complex[A]) = c[A] * rhs
  def /[A:Fractional:Trig](rhs:Complex[A]) = c[A] / rhs

  def /~[A:Fractional:Trig](rhs:Complex[A]) = c[A] /~ rhs
  def %[A:Fractional:Trig](rhs:Complex[A]) = c[A] % rhs
  def /%[A:Fractional:Trig](rhs:Complex[A]) = c[A] /% rhs

  def **[A:Fractional:Trig](rhs:Complex[A]) = c[A] ** rhs

  def +(rhs:Real) = Real(lhs) + rhs

  def /~(rhs:Int) = EuclideanRing[Int].quot(lhs, rhs)
  def /%(rhs:Int) = EuclideanRing[Int].quotmod(lhs, rhs)
  def pow(rhs:Int) = Ring[Int].pow(lhs, rhs)
  def **(rhs:Int) = Ring[Int].pow(lhs, rhs)
}

final class LiteralDoubleOps(lhs:Double) {
  def pow(rhs:Int) = Ring[Double].pow(lhs, rhs)
  def **(rhs:Int) = Ring[Double].pow(lhs, rhs)
}

object Implicits extends LowPriorityImplicits {
  implicit def eqOps[@spec(Int, Long, Float, Double) A:Eq](a:A) = new EqOps(a)
  implicit def orderOps[@spec(Int, Long, Float, Double) A:Order](a:A) = new OrderOps(a)
  implicit def semigroupOps[A:Semigroup](a:A) = new SemigroupOps(a)

  implicit def convertableOps[@spec(Int, Long, Float, Double) A:ConvertableFrom](a:A) = new ConvertableFromOps(a)

  implicit def ringOps[@spec(Int, Long, Float, Double) A:Ring](a:A) = new RingOps(a)
  implicit def euclideanRingOps[@spec(Int, Long, Float, Double) A:EuclideanRing](a:A) = new EuclideanRingOps(a)
  implicit def fieldOps[@spec(Float, Double) A:Field](a:A) = new FieldOps(a)

  implicit def fractionalOps[@spec(Float, Double) A:Fractional](a:A) = new FractionalOps(a)

  implicit def signedOps[@spec(Float, Double, Int, Long) A: Signed](a: A) = new SignedOps(a)
  implicit def nrootOps[@spec(Int, Long) A: EuclideanRingWithNRoot](a: A) = new NRootOps(a)

  implicit def literalIntOps(lhs:Int) = new LiteralIntOps(lhs)
  implicit def literalDoubleOps(lhs:Double) = new LiteralDoubleOps(lhs)

  implicit def complexDoubleOps(a:Double) = new ComplexDoubleOps(a)
}
