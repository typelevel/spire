package numerics.math

import scala.{specialized => spec}

trait Eq[@spec(Int,Long,Double) A] {
  self =>

  def eq(x:A, y:A) = equiv(x, y)

  def equiv(x:A, y:A): Boolean
  def nequiv(x:A, y:A): Boolean

  def on[U](f:(U) => A) = Eq.by(f)(this)
}

trait AnonymousEq[@spec A] extends Eq[A] {
  protected[this] def eqv(x:A, y:A): Boolean
  def equiv(x:A, y:A) = eqv(x, y)
  def nequiv(x:A, y:A) = !eqv(x, y)
}

trait EqOps[@spec(Int,Long,Double) A] {
  val lhs:A
  val e:Eq[A]
  def ===(rhs:A) = e.equiv(lhs, rhs)
  def !==(rhs:A) = e.nequiv(lhs, rhs)
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

  def by[@spec T, @spec S](f:(T) => S)(implicit e:Eq[S]): Eq[T] = new AnonymousEq[T] {
    def eqv(x:T, y:T) = e.equiv(f(x), f(y))
  }
}

trait LowPriority {
  implicit def generic[@spec A]: Eq[A] = new GenericEq[A]
}

private[this] class GenericEq[@spec A] extends AnonymousEq[A] {
  def eqv(x:A, y:A) = x == y
}

trait IntEq extends Eq[Int] {
  def equiv(x:Int, y:Int) = x == y
  def nequiv(x:Int, y:Int) = x != y
}
trait LongEq extends Eq[Long] {
  def equiv(x:Long, y:Long) = x == y
  def nequiv(x:Long, y:Long) = x != y
}
trait FloatEq extends Eq[Float] {
  def equiv(x:Float, y:Float) = x == y
  def nequiv(x:Float, y:Float) = x != y
}
trait DoubleEq extends Eq[Double] {
  def equiv(x:Double, y:Double) = x == y
  def nequiv(x:Double, y:Double) = x != y
}
trait BigIntEq extends Eq[BigInt] {
  def equiv(x:BigInt, y:BigInt) = x == y
  def nequiv(x:BigInt, y:BigInt) = x != y
}
trait BigDecimalEq extends Eq[BigDecimal] {
  def equiv(x:BigDecimal, y:BigDecimal) = x == y
  def nequiv(x:BigDecimal, y:BigDecimal) = x != y
}
trait RationalEq extends Eq[Rational] {
  def equiv(x:Rational, y:Rational) = x == y
  def nequiv(x:Rational, y:Rational) = x != y
}
class ComplexEq[A](implicit f:Fractional[A]) extends Eq[Complex[A]] {
  def equiv(x:Complex[A], y:Complex[A]) = x == y
  def nequiv(x:Complex[A], y:Complex[A]) = x != y
}

