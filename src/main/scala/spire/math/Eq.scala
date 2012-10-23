package spire.math

import language.experimental.macros
import scala.{specialized => spec}

import spire.algebra.Zero
import spire.macrosk.Ops

trait Eq[@spec A] {
  def eqv(x:A, y:A): Boolean
  def neqv(x:A, y:A): Boolean = !eqv(x, y)

  def on[@spec B](f:B => A) = Eq.by(f)(this)
}

final class EqOps[A](lhs:A)(implicit ev:Eq[A]) {
  def ===(rhs:A) = macro Ops.binop[A, Boolean]
  def =!=(rhs:A) = macro Ops.binop[A, Boolean]
}

object Eq extends LowPriority {
  implicit object ByteEq extends ByteEq
  implicit object ShortEq extends ShortEq
  implicit object CharEq extends CharEq
  implicit object IntEq extends IntEq
  implicit object LongEq extends LongEq
  implicit object FloatEq extends FloatEq
  implicit object DoubleEq extends DoubleEq
  implicit object BigIntEq extends BigIntEq
  implicit object BigDecimalEq extends BigDecimalEq
  implicit object RationalEq extends RationalEq
  implicit def complexEq[A:Fractional] = new ComplexEq[A] {
    val f = Fractional[A]
  }
  implicit object RealEq extends RealEq

  def apply[A](implicit e:Eq[A]):Eq[A] = e

  def by[@spec A, @spec B](f:A => B)(implicit e:Eq[B]): Eq[A] = new Eq[A] {
    def eqv(x:A, y:A) = e.eqv(f(x), f(y))
  }
}

trait LowPriority {
  implicit def generic[@spec A]: Eq[A] = new GenericEq[A]
}

private[this] class GenericEq[@spec A] extends Eq[A] {
  def eqv(x:A, y:A) = x == y
}

trait ByteEq extends Eq[Byte] {
  def eqv(x:Byte, y:Byte) = x == y
  override def neqv(x:Byte, y:Byte) = x != y
}
trait ShortEq extends Eq[Short] {
  def eqv(x:Short, y:Short) = x == y
  override def neqv(x:Short, y:Short) = x != y
}
trait CharEq extends Eq[Char] {
  def eqv(x:Char, y:Char) = x == y
  override def neqv(x:Char, y:Char) = x != y
}
trait IntEq extends Eq[Int] {
  def eqv(x:Int, y:Int) = x == y
  override def neqv(x:Int, y:Int) = x != y
}
trait LongEq extends Eq[Long] {
  def eqv(x:Long, y:Long) = x == y
  override def neqv(x:Long, y:Long) = x != y
}
trait FloatEq extends Eq[Float] {
  def eqv(x:Float, y:Float) = x == y
  override def neqv(x:Float, y:Float) = x != y
}
trait DoubleEq extends Eq[Double] {
  def eqv(x:Double, y:Double) = x == y
  override def neqv(x:Double, y:Double) = x != y
}
trait BigIntEq extends Eq[BigInt] {
  def eqv(x:BigInt, y:BigInt) = x == y
  override def neqv(x:BigInt, y:BigInt) = x != y
}
trait BigDecimalEq extends Eq[BigDecimal] {
  def eqv(x:BigDecimal, y:BigDecimal) = x == y
  override def neqv(x:BigDecimal, y:BigDecimal) = x != y
}
trait RationalEq extends Eq[Rational] {
  def eqv(x:Rational, y:Rational) = x == y
  override def neqv(x:Rational, y:Rational) = x != y
}
trait ComplexEq[A] extends Eq[Complex[A]] {
  implicit def f: Fractional[A]
  def eqv(x:Complex[A], y:Complex[A]) = x == y
  override def neqv(x:Complex[A], y:Complex[A]) = x != y
}
trait RealEq extends Eq[Real] {
  def eqv(x: Real, y: Real) = (x - y).sign == Zero
  override def neqv(x: Real, y: Real) = (x - y).sign != Zero
}
