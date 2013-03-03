package spire.algebra

import scala.{specialized => spec}

import spire.math._
import spire.macrosk.Ops

import java.lang.Math

import scala.collection.SeqLike
import scala.reflect.ClassTag

trait Order[@spec A] extends Eq[A] {
  self =>

  def gt(x: A, y: A): Boolean = compare(x, y) > 0
  def lt(x: A, y: A): Boolean = compare(x, y) < 0
  def gteqv(x: A, y: A): Boolean = compare(x, y) >= 0
  def lteqv(x: A, y: A): Boolean = compare(x, y) <= 0

  def min(x: A, y: A): A = if (lt(x, y)) x else y
  def max(x: A, y: A): A = if (gt(x, y)) x else y
  def compare(x: A, y: A): Int

  override def on[@spec B](f: B => A): Order[B] = new MappedOrder(this)(f)
  def reverse: Order[A] = new ReversedOrder(this)
}

class MappedOrder[@spec A, @spec B](order: Order[B])(f: A => B) extends Order[A] {
  def eqv(x: A, y: A) = order.eqv(f(x), f(y))
  def compare(x: A, y: A) = order.compare(f(x), f(y))
}

class ReversedOrder[@spec A](order: Order[A]) extends Order[A] {
  def eqv(x: A, y: A) = order.eqv(x, y)
  def compare(x: A, y: A) = order.compare(y, x)
}

final class OrderOps[A](lhs: A)(implicit ev: Order[A]) {
  def >(rhs: A) = macro Ops.binop[A, Boolean]
  def >=(rhs: A) = macro Ops.binop[A, Boolean]
  def <(rhs: A) = macro Ops.binop[A, Boolean]
  def <=(rhs: A) = macro Ops.binop[A, Boolean]
  def compare(rhs: A) = macro Ops.binop[A, Int]
  def min(rhs: A) = macro Ops.binop[A, A]
  def max(rhs: A) = macro Ops.binop[A, A]

  def >(rhs: Int)(implicit c: ConvertableTo[A]) = ev.gt(lhs, c.fromInt(rhs))
  def >=(rhs: Int)(implicit c: ConvertableTo[A]) = ev.gteqv(lhs, c.fromInt(rhs))
  def <(rhs: Int)(implicit c: ConvertableTo[A]) = ev.lt(lhs, c.fromInt(rhs))
  def <=(rhs: Int)(implicit c: ConvertableTo[A]) = ev.lteqv(lhs, c.fromInt(rhs))
  def compare(rhs: Int)(implicit c: ConvertableTo[A]) = ev.compare(lhs, c.fromInt(rhs))
  def min(rhs: Int)(implicit c: ConvertableTo[A]) = ev.min(lhs, c.fromInt(rhs))
  def max(rhs: Int)(implicit c: ConvertableTo[A]) = ev.max(lhs, c.fromInt(rhs))

  def >(rhs: Double)(implicit c: ConvertableTo[A]) = ev.gt(lhs, c.fromDouble(rhs))
  def >=(rhs: Double)(implicit c: ConvertableTo[A]) = ev.gteqv(lhs, c.fromDouble(rhs))
  def <(rhs: Double)(implicit c: ConvertableTo[A]) = ev.lt(lhs, c.fromDouble(rhs))
  def <=(rhs: Double)(implicit c: ConvertableTo[A]) = ev.lteqv(lhs, c.fromDouble(rhs))
  def compare(rhs: Double)(implicit c: ConvertableTo[A]) = ev.compare(lhs, c.fromDouble(rhs))
  def min(rhs: Double)(implicit c: ConvertableTo[A]) = ev.min(lhs, c.fromDouble(rhs))
  def max(rhs: Double)(implicit c: ConvertableTo[A]) = ev.max(lhs, c.fromDouble(rhs))

  def >(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) > rhs
  def >=(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) >= rhs
  def <(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) < rhs
  def <=(rhs:Number)(implicit c:ConvertableFrom[A]): Boolean = c.toNumber(lhs) <= rhs
  def compare(rhs:Number)(implicit c:ConvertableFrom[A]): Int = c.toNumber(lhs) compare rhs
  def min(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) min rhs
  def max(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) max rhs
}

object Order {
  implicit object CharOrder extends CharOrder
  implicit object UByteOrder extends UByteOrder
  implicit object UShortOrder extends UShortOrder
  implicit object UIntOrder extends UIntOrder
  implicit object ULongOrder extends ULongOrder
  implicit object RealOrder extends RealOrder
  implicit object SafeLongOrder extends SafeLongOrder
  implicit object NaturalOrder extends NaturalOrder
  implicit object NumberOrder extends NumberOrder

  def by[@spec A, @spec B](f: A => B)(implicit o: Order[B]): Order[A] = o.on(f)

  def from[@spec A](f: (A, A) => Int): Order[A] = new Order[A] {
    def eqv(x: A, y: A) = f(x, y) == 0
    def compare(x: A, y: A) = f(x, y)
  }

  def apply[A](implicit o: Order[A]) = o

  implicit def ordering[A](implicit o: Order[A]) = new Ordering[A] {
    def compare(x: A, y: A) = o.compare(x, y)
  }
}

trait CharOrder extends Order[Char] with CharEq {
  override def gt(x: Char, y: Char) = x > y
  override def gteqv(x: Char, y: Char) = x >= y
  override def lt(x: Char, y: Char) = x < y
  override def lteqv(x: Char, y: Char) = x <= y
  def compare(x: Char, y: Char) = if (x < y) -1 else if (x > y) 1 else 0
}

trait UByteOrder extends Order[UByte] with UByteEq {
  override def gt(x: UByte, y: UByte) = x > y
  override def gteqv(x: UByte, y: UByte) = x >= y
  override def lt(x: UByte, y: UByte) = x < y
  override def lteqv(x: UByte, y: UByte) = x <= y
  def compare(x: UByte, y: UByte) = if (x < y) -1 else if (x > y) 1 else 0
}

trait UShortOrder extends Order[UShort] with UShortEq {
  override def gt(x: UShort, y: UShort) = x > y
  override def gteqv(x: UShort, y: UShort) = x >= y
  override def lt(x: UShort, y: UShort) = x < y
  override def lteqv(x: UShort, y: UShort) = x <= y
  def compare(x: UShort, y: UShort) = if (x < y) -1 else if (x > y) 1 else 0
}

trait UIntOrder extends Order[UInt] with UIntEq {
  override def gt(x: UInt, y: UInt) = x > y
  override def gteqv(x: UInt, y: UInt) = x >= y
  override def lt(x: UInt, y: UInt) = x < y
  override def lteqv(x: UInt, y: UInt) = x <= y
  def compare(x: UInt, y: UInt) = if (x < y) -1 else if (x > y) 1 else 0
}

trait ULongOrder extends Order[ULong] with ULongEq {
  override def gt(x: ULong, y: ULong) = x > y
  override def gteqv(x: ULong, y: ULong) = x >= y
  override def lt(x: ULong, y: ULong) = x < y
  override def lteqv(x: ULong, y: ULong) = x <= y
  def compare(x: ULong, y: ULong) = if (x < y) -1 else if (x > y) 1 else 0
}

trait RealOrder extends Order[Real] with RealEq {
  def compare(x: Real, y: Real) = (x - y).signum
}

trait SafeLongOrder extends Order[SafeLong] with SafeLongEq {
  override def gt(x: SafeLong, y: SafeLong) = x > y
  override def gteqv(x: SafeLong, y: SafeLong) = x >= y
  override def lt(x: SafeLong, y: SafeLong) = x < y
  override def lteqv(x: SafeLong, y: SafeLong) = x <= y
  def compare(x: SafeLong, y: SafeLong) = if (x < y) -1 else if (x > y) 1 else 0
}

trait NaturalOrder extends Order[Natural] with NaturalEq {
  override def gt(x: Natural, y: Natural) = x > y
  override def gteqv(x: Natural, y: Natural) = x >= y
  override def lt(x: Natural, y: Natural) = x < y
  override def lteqv(x: Natural, y: Natural) = x <= y
  def compare(x: Natural, y: Natural) = x.compare(y)
}

trait NumberOrder extends Order[Number] with NumberEq {
  override def gt(x: Number, y: Number) = x > y
  override def gteqv(x: Number, y: Number) = x >= y
  override def lt(x: Number, y: Number) = x < y
  override def lteqv(x: Number, y: Number) = x <= y
  def compare(x: Number, y: Number) = x.compare(y)
}
