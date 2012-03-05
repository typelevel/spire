package numerics.math

import scala.{specialized => spec}

trait Order[@spec A] extends Eq[A] {
  self =>

  def gt(x:A, y:A): Boolean
  def lt(x:A, y:A): Boolean
  def gteq(x:A, y:A): Boolean
  def lteq(x:A, y:A): Boolean

  def min(x:A, y:A): A = if (lt(x, y)) x else y
  def max(x:A, y:A): A = if (gt(x, y)) x else y
  def compare(x:A, y:A): Int = if (lt(x, y)) -1 else if (gt(x, y)) 1 else 0

  override def on[@spec B](f:B => A): Order[B] = new AnonymousOrder[B] {
    def cmp(x:B, y:B) = self.compare(f(x), f(y))
  }
  def reverse: Order[A] = new AnonymousOrder[A] {
    def cmp(x:A, y:A) = self.compare(y, x)
  }
}

trait AnonymousOrder[A] extends Order[A] {
  protected[this] def cmp(x:A, y:A): Int

  def eq(x:A, y:A) = cmp(x, y) == 0
  def neq(x:A, y:A) = cmp(x, y) != 0
  def gt(x:A, y:A) = cmp(x, y) > 0
  def lt(x:A, y:A) = cmp(x, y) < 0
  def gteq(x:A, y:A) = cmp(x, y) > -1
  def lteq(x:A, y:A) = cmp(x, y) < 1
  override def compare(x:A, y:A) = cmp(x, y)
}

final class OrderOps[@spec A](lhs:A)(implicit ev:Order[A]) {
  def >(rhs:A) = ev.gt(lhs, rhs)
  def >=(rhs:A) = ev.gteq(lhs, rhs)
  def <(rhs:A) = ev.lt(lhs, rhs)
  def <=(rhs:A) = ev.lteq(lhs, rhs)

  def cmp(rhs:A) = ev.compare(lhs, rhs)
  def min(rhs:A) = ev.min(lhs, rhs)
  def max(rhs:A) = ev.max(lhs, rhs)
}

object Order {
  implicit object IntOrder extends IntOrder
  implicit object LongOrder extends LongOrder
  implicit object FloatOrder extends FloatOrder
  implicit object DoubleOrder extends DoubleOrder
  implicit object BigIntOrder extends BigIntOrder
  implicit object BigDecimalOrder extends BigDecimalOrder
  implicit object RationalOrder extends RationalOrder
  implicit object RealOrder extends RealOrder

  def by[@spec A, @spec B](f:A => B)(implicit o:Order[B]): Order[A] = o.on(f)

  def apply[A](implicit o:Order[A]) = o

  implicit def ordering[A](implicit o:Order[A]) = new Ordering[A] {
    def compare(x:A, y:A) = o.compare(x, y)
  }
}

trait IntOrder extends Order[Int] with IntEq {
  def gt(x:Int, y:Int) = x > y
  def gteq(x:Int, y:Int) = x >= y
  def lt(x:Int, y:Int) = x < y
  def lteq(x:Int, y:Int) = x <= y
}

trait LongOrder extends Order[Long] with LongEq {
  def gt(x:Long, y:Long) = x > y
  def gteq(x:Long, y:Long) = x >= y
  def lt(x:Long, y:Long) = x < y
  def lteq(x:Long, y:Long) = x <= y
}

trait FloatOrder extends Order[Float] with FloatEq {
  def gt(x:Float, y:Float) = x > y
  def gteq(x:Float, y:Float) = x >= y
  def lt(x:Float, y:Float) = x < y
  def lteq(x:Float, y:Float) = x <= y
}

trait DoubleOrder extends Order[Double] with DoubleEq {
  def gt(x:Double, y:Double) = x > y
  def gteq(x:Double, y:Double) = x >= y
  def lt(x:Double, y:Double) = x < y
  def lteq(x:Double, y:Double) = x <= y
}

trait BigIntOrder extends Order[BigInt] with BigIntEq {
  def gt(x:BigInt, y:BigInt) = x > y
  def gteq(x:BigInt, y:BigInt) = x >= y
  def lt(x:BigInt, y:BigInt) = x < y
  def lteq(x:BigInt, y:BigInt) = x <= y
}

trait BigDecimalOrder extends Order[BigDecimal] with BigDecimalEq {
  def gt(x:BigDecimal, y:BigDecimal) = x > y
  def gteq(x:BigDecimal, y:BigDecimal) = x >= y
  def lt(x:BigDecimal, y:BigDecimal) = x < y
  def lteq(x:BigDecimal, y:BigDecimal) = x <= y
}

trait RationalOrder extends Order[Rational] with RationalEq {
  def gt(x:Rational, y:Rational) = x > y
  def gteq(x:Rational, y:Rational) = x >= y
  def lt(x:Rational, y:Rational) = x < y
  def lteq(x:Rational, y:Rational) = x <= y
}

trait RealOrder extends Order[Real] with RealEq {
  private def cmp(x: Real, y: Real): Int = (x - y).signum
  def gt(x:Real, y:Real) = cmp(x, y) > 0
  def gteq(x:Real, y:Real) = cmp(x, y) >= 0
  def lt(x:Real, y:Real) = cmp(x, y) < 0
  def lteq(x:Real, y:Real) = cmp(x, y) <= 0
}

