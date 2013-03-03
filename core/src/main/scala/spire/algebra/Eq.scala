package spire.algebra

import scala.{specialized => spec}

import spire.math._
import spire.macrosk.Ops

trait Eq[@spec A] {
  def eqv(x:A, y:A): Boolean
  def neqv(x:A, y:A): Boolean = !eqv(x, y)
  def on[@spec B](f:B => A): Eq[B] = new MappedEq(this)(f)
}

class MappedEq[@spec A, @spec B](eq: Eq[B])(f: A => B) extends Eq[A] {
  def eqv(x: A, y: A): Boolean = eq.eqv(f(x), f(x))
}

final class EqOps[A](lhs:A)(implicit ev:Eq[A]) {
  def ===(rhs:A) = macro Ops.binop[A, Boolean]
  def =!=(rhs:A) = macro Ops.binop[A, Boolean]
}

object Eq {
  implicit object CharEq extends CharEq
  implicit object UByteEq extends UByteEq
  implicit object UShortEq extends UShortEq
  implicit object UIntEq extends UIntEq
  implicit object ULongEq extends ULongEq
  implicit object RationalEq extends RationalEq
  implicit def complexEq[A:Fractional] = new ComplexEq[A] {}
  implicit def gaussianEq[A: Integral] = new GaussianEq[A] {}
  implicit object RealEq extends RealEq
  implicit object SafeLongEq extends SafeLongEq
  implicit object NaturalEq extends NaturalEq
  implicit object NumberEq extends NumberEq

  def apply[A](implicit e:Eq[A]):Eq[A] = e

  def by[@spec A, @spec B](f:A => B)(implicit e:Eq[B]): Eq[A] = new MappedEq(e)(f)
}

trait CharEq extends Eq[Char] {
  def eqv(x:Char, y:Char) = x == y
  override def neqv(x:Char, y:Char) = x != y
}
trait UByteEq extends Eq[UByte] {
  def eqv(x:UByte, y:UByte) = x == y
  override def neqv(x:UByte, y:UByte) = x != y
}
trait UShortEq extends Eq[UShort] {
  def eqv(x:UShort, y:UShort) = x == y
  override def neqv(x:UShort, y:UShort) = x != y
}
trait UIntEq extends Eq[UInt] {
  def eqv(x:UInt, y:UInt) = x == y
  override def neqv(x:UInt, y:UInt) = x != y
}
trait ULongEq extends Eq[ULong] {
  def eqv(x:ULong, y:ULong) = x == y
  override def neqv(x:ULong, y:ULong) = x != y
}
trait RationalEq extends Eq[Rational] {
  def eqv(x:Rational, y:Rational) = x == y
  override def neqv(x:Rational, y:Rational) = x != y
}
trait ComplexEq[A] extends Eq[Complex[A]] {
  def eqv(x:Complex[A], y:Complex[A]) = x eqv y
  override def neqv(x:Complex[A], y:Complex[A]) = x neqv y
}
trait GaussianEq[A] extends Eq[Gaussian[A]] {
  def eqv(x:Gaussian[A], y:Gaussian[A]) = x eqv y
  override def neqv(x:Gaussian[A], y:Gaussian[A]) = x neqv y
}
trait RealEq extends Eq[Real] {
  def eqv(x: Real, y: Real) = (x - y).sign == Zero
  override def neqv(x: Real, y: Real) = (x - y).sign != Zero
}
trait SafeLongEq extends Eq[SafeLong] {
  def eqv(x: SafeLong, y: SafeLong) = x == y
  override def neqv(x: SafeLong, y: SafeLong) = x != y
}
trait NumberEq extends Eq[Number] {
  def eqv(x: Number, y: Number) = x == y
  override def neqv(x: Number, y: Number) = x != y
}
trait NaturalEq extends Eq[Natural] {
  def eqv(x: Natural, y: Natural) = x == y
  override def neqv(x: Natural, y: Natural) = x != y
}
