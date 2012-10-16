package spire

import scala.{specialized => spec}
import language.implicitConversions
import language.experimental.macros

import spire.algebra._
import spire.math._

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

  @inline private def c[A](implicit f:Fractional[A], t:Trig[A]) =
    Complex(f.fromInt(lhs), f.zero)

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

  @inline private final def c[A:Fractional:ConvertableTo:Trig]:Complex[A] =
    Complex(ConvertableFrom[Double].toType[A](lhs), Fractional[A].zero)

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

object implicits {
  implicit def eqOps[@spec(Int, Long, Float, Double) A:Eq](a:A) = new EqOps(a)
  implicit def orderOps[@spec(Int, Long, Float, Double) A:Order](a:A) = new OrderOps(a)
  implicit def semigroupOps[A:Semigroup](a:A) = new SemigroupOps(a)
  implicit def groupOps[A:Group](a:A) = new GroupOps(a)

  implicit def convertableOps[@spec(Int, Long, Float, Double) A:ConvertableFrom](a:A) = new ConvertableFromOps(a)

  implicit def ringOps[@spec(Int, Long, Float, Double) A:Ring](a:A) = new RingOps(a)
  implicit def euclideanRingOps[@spec(Int, Long, Float, Double) A:EuclideanRing](a:A) = new EuclideanRingOps(a)
  implicit def fieldOps[@spec(Float, Double) A:Field](a:A) = new FieldOps(a)

  implicit def integralOps[@spec(Float, Double) A:Integral](a:A) = new IntegralOps(a)
  implicit def fractionalOps[@spec(Float, Double) A:Fractional](a:A) = new FractionalOps(a)

  implicit def signedOps[@spec(Float, Double, Int, Long) A: Signed](a: A) = new SignedOps(a)
  implicit def nrootOps[@spec(Int, Long) A: NRoot](a: A) = new NRootOps(a)

  implicit def literalIntOps(lhs:Int) = new LiteralIntOps(lhs)
  implicit def literalDoubleOps(lhs:Double) = new LiteralDoubleOps(lhs)
}

object syntax {
  import spire.macrosk._
  def cfor[A](init:A)(test:A => Boolean, next:A => A)(body:A => Unit): Unit =
    macro Syntax.cforMacro[A]

  implicit def literals(s:StringContext) = new Literals(s)

  object radix { implicit def radix(s:StringContext) = new Radix(s) }
  object si { implicit def siLiterals(s:StringContext) = new SiLiterals(s) }
  object us { implicit def usLiterals(s:StringContext) = new UsLiterals(s) }
  object eu { implicit def euLiterals(s:StringContext) = new EuLiterals(s) }
}
