package spire.algebra

import scala.{specialized => spec}

import spire.math.{ ConvertableFrom, ConvertableTo, Number }
import spire.macrosk.Ops

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
