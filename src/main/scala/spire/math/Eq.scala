package spire.math

import scala.{specialized => spec}

trait Eq[@spec A] {
  def eq(x:A, y:A): Boolean
  def neq(x:A, y:A): Boolean

  def on[@spec B](f:B => A) = Eq.by(f)(this)
}

trait AnonymousEq[@spec A] extends Eq[A] {
  def neq(x:A, y:A) = !eq(x, y)
}

final class EqOps[@spec(Int,Long,Double) A](lhs:A)(implicit ev:Eq[A]) {
  def ===(rhs:A) = ev.eq(lhs, rhs)
  def =!=(rhs:A) = ev.neq(lhs, rhs)
}

object Eq extends LowPriority {
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

  def by[@spec A, @spec B](f:A => B)(implicit e:Eq[B]): Eq[A] = new AnonymousEq[A] {
    def eq(x:A, y:A) = e.eq(f(x), f(y))
  }
}

trait LowPriority {
  implicit def generic[@spec A]: Eq[A] = new GenericEq[A]
}

private[this] class GenericEq[@spec A] extends AnonymousEq[A] {
  def eq(x:A, y:A) = x == y
}

trait IntEq extends Eq[Int] {
  def eq(x:Int, y:Int) = x == y
  def neq(x:Int, y:Int) = x != y
}
trait LongEq extends Eq[Long] {
  def eq(x:Long, y:Long) = x == y
  def neq(x:Long, y:Long) = x != y
}
trait FloatEq extends Eq[Float] {
  def eq(x:Float, y:Float) = x == y
  def neq(x:Float, y:Float) = x != y
}
trait DoubleEq extends Eq[Double] {
  def eq(x:Double, y:Double) = x == y
  def neq(x:Double, y:Double) = x != y
}
trait BigIntEq extends Eq[BigInt] {
  def eq(x:BigInt, y:BigInt) = x == y
  def neq(x:BigInt, y:BigInt) = x != y
}
trait BigDecimalEq extends Eq[BigDecimal] {
  def eq(x:BigDecimal, y:BigDecimal) = x == y
  def neq(x:BigDecimal, y:BigDecimal) = x != y
}
trait RationalEq extends Eq[Rational] {
  def eq(x:Rational, y:Rational) = x == y
  def neq(x:Rational, y:Rational) = x != y
}
class ComplexEq[A](implicit f:Fractional[A]) extends Eq[Complex[A]] {
  def eq(x:Complex[A], y:Complex[A]) = x == y
  def neq(x:Complex[A], y:Complex[A]) = x != y
}
trait RealEq extends Eq[Real] {
  def eq(x: Real, y: Real) = (x - y).sign == Zero
  def neq(x: Real, y: Real) = (x - y).sign != Zero
}
