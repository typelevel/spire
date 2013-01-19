package spire.algebra

import scala.{ specialized => spec }
import spire.macrosk.Ops

object Additive {
  def apply[A](s: Semigroup[A]): AdditiveSemigroup[A] =
    new AdditiveSemigroup[A] {
      def plus(x: A, y: A): A = s.op(x, y)
    }
  def apply[A](m: Monoid[A]): AdditiveMonoid[A] =
    new AdditiveMonoid[A] {
      def plus(x: A, y: A): A = m.op(x, y)
      def zero = m.id
    }
  def apply[A](g: Group[A]): AdditiveGroup[A] =
    new AdditiveGroup[A] {
      def plus(x: A, y: A): A = g.op(x, y)
      override def minus(x: A, y: A): A = g.op(x, g.inverse(y))
      def zero: A = g.id
      def negate(x: A): A = g.inverse(x)
    }
  def apply[A](g: AbGroup[A]): AdditiveAbGroup[A] =
    new AdditiveAbGroup[A] {
      def plus(x: A, y: A): A = g.op(x, y)
      override def minus(x: A, y: A): A = g.op(x, g.inverse(y))
      def zero: A = g.id
      def negate(x: A): A = g.inverse(x)
    }
}

trait AdditiveSemigroup[@spec(Int,Long,Float,Double) A] {
  def additive: Semigroup[A] = new Semigroup[A] {
    def op(x: A, y: A): A = plus(x, y)
  }

  def plus(x: A, y: A): A
}

trait AdditiveMonoid[@spec(Int,Long,Float,Double) A] extends AdditiveSemigroup[A] {
  override def additive: Monoid[A] = new Monoid[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
  }

  def zero: A
}

trait AdditiveGroup[@spec(Int,Long,Float,Double) A] extends AdditiveMonoid[A] {
  override def additive: Group[A] = new Group[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
    def inverse(x: A): A = negate(x)
  }

  def negate(x: A): A
  def minus(x: A, y: A): A = plus(x, negate(y))
}

trait AdditiveAbGroup[@spec(Int,Long,Float,Double) A] extends AdditiveGroup[A] {
  override def additive: AbGroup[A] = new AbGroup[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
    def inverse(x: A): A = negate(x)
  }
}

object AdditiveSemigroup extends AdditiveSemigroup1
object AdditiveMonoid extends AdditiveMonoid0
object AdditiveGroup extends AdditiveGroup1

trait AdditiveSemigroup0 {
  implicit def MonoidIsSemigroup[@spec(Int,Long,Float,Double) A](implicit
    m: AdditiveMonoid[A]): AdditiveSemigroup[A] = m
}

trait AdditiveSemigroup1 extends AdditiveSemigroup0 {
  implicit def ringIsAdditiveSemigroup[@spec(Int, Long, Float, Double) A](implicit
    r: Ring[A]): AdditiveSemigroup[A] = r
}

trait AdditiveMonoid0 {
  implicit def GroupIsMonoid[@spec(Int,Long,Float,Double) A](implicit
    g: AdditiveGroup[A]): AdditiveMonoid[A] = g
}

trait AdditiveGroup0 {
  implicit def moduleIsAdditiveGroup[V, @spec(Int, Long, Float, Double) R](implicit
    m: Module[V, R]): AdditiveGroup[V] = m
}

trait AdditiveGroup1 extends AdditiveGroup0 {
  implicit def AbGroupIsGroup[@spec(Int,Long,Float,Double) A](implicit
    g: AdditiveAbGroup[A]): AdditiveGroup[A] = g
}

import spire.math.{ConvertableTo, ConvertableFrom, Number}

final class AdditiveSemigroupOps[A](lhs:A)(implicit ev:AdditiveSemigroup[A]) {
  def +(rhs:A): A = macro Ops.binop[A, A]
  def +(rhs:Int)(implicit c: Ring[A]): A = ev.plus(lhs, c.fromInt(rhs))
  def +(rhs:Double)(implicit c:ConvertableTo[A]): A = ev.plus(lhs, c.fromDouble(rhs))
  def +(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) + rhs
}

final class AdditiveGroupOps[A](lhs:A)(implicit ev:AdditiveGroup[A]) {
  def unary_-() = macro Ops.unop[A]
  def -(rhs:A): A = macro Ops.binop[A, A]
  def -(rhs:Int)(implicit c: Ring[A]): A = ev.minus(lhs, c.fromInt(rhs))
  def -(rhs:Double)(implicit c:ConvertableTo[A]): A = ev.minus(lhs, c.fromDouble(rhs))
  def -(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) - rhs
}
