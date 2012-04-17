package spire.math

import scala.{specialized => spec}

trait Order[@spec A] extends Eq[A] {
  self =>

  def gt(x:A, y:A): Boolean = compare(x, y) > 0
  def lt(x:A, y:A): Boolean = compare(x, y) < 0
  def gteqv(x:A, y:A): Boolean = compare(x, y) >= 0
  def lteqv(x:A, y:A): Boolean = compare(x, y) <= 0

  def min(x:A, y:A): A = if (lt(x, y)) x else y
  def max(x:A, y:A): A = if (gt(x, y)) x else y
  def compare(x:A, y:A): Int

  override def on[@spec B](f:B => A): Order[B] = new Order[B] {
    def eqv(x:B, y:B) = self.eq(f(x), f(y))
    def compare(x:B, y:B) = self.compare(f(x), f(y))
  }
  def reverse: Order[A] = new Order[A] {
    def eqv(x:A, y:A) = self.eq(y, x)
    def compare(x:A, y:A) = self.compare(y, x)
  }
}

final class OrderOps[@spec A](lhs:A)(implicit ev:Order[A]) {
  def >(rhs:A) = ev.gt(lhs, rhs)
  def >=(rhs:A) = ev.gteqv(lhs, rhs)
  def <(rhs:A) = ev.lt(lhs, rhs)
  def <=(rhs:A) = ev.lteqv(lhs, rhs)

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
  override def gt(x:Int, y:Int) = x > y
  override def gteqv(x:Int, y:Int) = x >= y
  override def lt(x:Int, y:Int) = x < y
  override def lteqv(x:Int, y:Int) = x <= y
  def compare(x:Int, y:Int) = if (x < y) -1 else if (x > y) 1 else 0
}

trait LongOrder extends Order[Long] with LongEq {
  override def gt(x:Long, y:Long) = x > y
  override def gteqv(x:Long, y:Long) = x >= y
  override def lt(x:Long, y:Long) = x < y
  override def lteqv(x:Long, y:Long) = x <= y
  def compare(x:Long, y:Long) = if (x < y) -1 else if (x > y) 1 else 0
}

trait FloatOrder extends Order[Float] with FloatEq {
  override def gt(x:Float, y:Float) = x > y
  override def gteqv(x:Float, y:Float) = x >= y
  override def lt(x:Float, y:Float) = x < y
  override def lteqv(x:Float, y:Float) = x <= y
  def compare(x:Float, y:Float) = if (x < y) -1 else if (x > y) 1 else 0
}

trait DoubleOrder extends Order[Double] with DoubleEq {
  override def gt(x:Double, y:Double) = x > y
  override def gteqv(x:Double, y:Double) = x >= y
  override def lt(x:Double, y:Double) = x < y
  override def lteqv(x:Double, y:Double) = x <= y
  def compare(x:Double, y:Double) = if (x < y) -1 else if (x > y) 1 else 0
}

trait BigIntOrder extends Order[BigInt] with BigIntEq {
  override def gt(x:BigInt, y:BigInt) = x > y
  override def gteqv(x:BigInt, y:BigInt) = x >= y
  override def lt(x:BigInt, y:BigInt) = x < y
  override def lteqv(x:BigInt, y:BigInt) = x <= y
  def compare(x:BigInt, y:BigInt) = if (x < y) -1 else if (x > y) 1 else 0
}

trait BigDecimalOrder extends Order[BigDecimal] with BigDecimalEq {
  override def gt(x:BigDecimal, y:BigDecimal) = x > y
  override def gteqv(x:BigDecimal, y:BigDecimal) = x >= y
  override def lt(x:BigDecimal, y:BigDecimal) = x < y
  override def lteqv(x:BigDecimal, y:BigDecimal) = x <= y
  def compare(x:BigDecimal, y:BigDecimal) = if (x < y) -1 else if (x > y) 1 else 0
}

trait RationalOrder extends Order[Rational] with RationalEq {
  override def gt(x:Rational, y:Rational) = x > y
  override def gteqv(x:Rational, y:Rational) = x >= y
  override def lt(x:Rational, y:Rational) = x < y
  override def lteqv(x:Rational, y:Rational) = x <= y
  def compare(x:Rational, y:Rational) = if (x < y) -1 else if (x > y) 1 else 0
}

trait RealOrder extends Order[Real] with RealEq {
  override def gt(x:Real, y:Real) = compare(x, y) > 0
  override def gteqv(x:Real, y:Real) = compare(x, y) >= 0
  override def lt(x:Real, y:Real) = compare(x, y) < 0
  override def lteqv(x:Real, y:Real) = compare(x, y) <= 0
  def compare(x:Real, y:Real) = (x - y).signum
}

