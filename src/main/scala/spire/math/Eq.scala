package spire.math

import spire.algebra.{ Zero }
import scala.{specialized => spec}

trait Eq[@spec A] {
  def eq(x:A, y:A): Boolean
  def neq(x:A, y:A): Boolean = !eq(x, y)

  def on[@spec B](f:B => A) = Eq.by(f)(this)
}

final class EqOps[@spec(Int,Long,Double) A](lhs:A)(implicit ev:Eq[A]) {
  def ===(rhs:A) = ev.eq(lhs, rhs)
  def =!=(rhs:A) = ev.neq(lhs, rhs)
}

object Eq extends LowPriority {
  implicit object ByteEq extends ByteEq
  implicit object ShortEq extends ShortEq
  implicit object IntEq extends IntEq
  implicit object LongEq extends LongEq
  implicit object FloatEq extends FloatEq
  implicit object DoubleEq extends DoubleEq
  implicit object BigIntEq extends BigIntEq
  implicit object BigDecimalEq extends BigDecimalEq
  implicit object RationalEq extends RationalEq
  implicit def complexEq[A:Fractional] = new ComplexEq
  implicit object RealEq extends RealEq

  def apply[A](implicit e:Eq[A]):Eq[A] = e

  def by[@spec A, @spec B](f:A => B)(implicit e:Eq[B]): Eq[A] = new Eq[A] {
    def eq(x:A, y:A) = e.eq(f(x), f(y))
  }
}

trait LowPriority {
  implicit def generic[@spec A]: Eq[A] = new GenericEq[A]
}

private[this] class GenericEq[@spec A] extends Eq[A] {
  def eq(x:A, y:A) = x == y
}

trait ByteEq extends Eq[Byte] {
  def eq(x:Byte, y:Byte) = x == y
  override def neq(x:Byte, y:Byte) = x != y
}
trait ShortEq extends Eq[Short] {
  def eq(x:Short, y:Short) = x == y
  override def neq(x:Short, y:Short) = x != y
}
trait IntEq extends Eq[Int] {
  def eq(x:Int, y:Int) = x == y
  override def neq(x:Int, y:Int) = x != y
}
trait LongEq extends Eq[Long] {
  def eq(x:Long, y:Long) = x == y
  override def neq(x:Long, y:Long) = x != y
}
trait FloatEq extends Eq[Float] {
  def eq(x:Float, y:Float) = x == y
  override def neq(x:Float, y:Float) = x != y
}
trait DoubleEq extends Eq[Double] {
  def eq(x:Double, y:Double) = x == y
  override def neq(x:Double, y:Double) = x != y
}
trait BigIntEq extends Eq[BigInt] {
  def eq(x:BigInt, y:BigInt) = x == y
  override def neq(x:BigInt, y:BigInt) = x != y
}
trait BigDecimalEq extends Eq[BigDecimal] {
  def eq(x:BigDecimal, y:BigDecimal) = x == y
  override def neq(x:BigDecimal, y:BigDecimal) = x != y
}
trait RationalEq extends Eq[Rational] {
  def eq(x:Rational, y:Rational) = x == y
  override def neq(x:Rational, y:Rational) = x != y
}
class ComplexEq[A](implicit f:Fractional[A]) extends Eq[Complex[A]] {
  def eq(x:Complex[A], y:Complex[A]) = x == y
  override def neq(x:Complex[A], y:Complex[A]) = x != y
}
trait RealEq extends Eq[Real] {
  def eq(x: Real, y: Real) = (x - y).sign == Zero
  override def neq(x: Real, y: Real) = (x - y).sign != Zero
}
