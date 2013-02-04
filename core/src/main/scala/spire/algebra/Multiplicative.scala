package spire.algebra

import scala.{ specialized => spec }
import spire.macrosk.Ops

object Multiplicative {
  def apply[A](s: Semigroup[A]): MultiplicativeSemigroup[A] =
    new MultiplicativeSemigroup[A] {
      def times(x: A, y: A): A = s.op(x, y)
    }
  def apply[A](m: Monoid[A]): MultiplicativeMonoid[A] =
    new MultiplicativeMonoid[A] {
      def times(x: A, y: A): A = m.op(x, y)
      def one = m.id
    }
  def apply[A](g: Group[A]): MultiplicativeGroup[A] =
    new MultiplicativeGroup[A] {
      def times(x: A, y: A): A = g.op(x, y)
      def div(x: A, y: A): A = g.op(x, g.inverse(y))
      def one: A = g.id
      override def reciprocal(x: A): A = g.inverse(x)
    }
  def apply[A](g: AbGroup[A]): MultiplicativeAbGroup[A] =
    new MultiplicativeAbGroup[A] {
      def times(x: A, y: A): A = g.op(x, y)
      def div(x: A, y: A): A = g.op(x, g.inverse(y))
      def one: A = g.id
      override def reciprocal(x: A): A = g.inverse(x)
    }
}

trait MultiplicativeSemigroup[@spec(Int,Long,Float,Double) A] {
  def multiplicative: Semigroup[A] = new Semigroup[A] {
    def op(x: A, y: A): A = times(x, y)
  }

  def times(x: A, y: A): A
}

trait MultiplicativeMonoid[@spec(Int,Long,Float,Double) A] extends MultiplicativeSemigroup[A] {
  override def multiplicative: Monoid[A] = new Monoid[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
  }

  def one: A
}

trait MultiplicativeGroup[@spec(Int,Long,Float,Double) A] extends MultiplicativeMonoid[A] {
  override def multiplicative: Group[A] = new Group[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
    def inverse(x: A): A = reciprocal(x)
  }

  def reciprocal(x: A): A = div(one, x)
  def div(x: A, y: A): A
}

trait MultiplicativeAbGroup[@spec(Int,Long,Float,Double) A] extends MultiplicativeGroup[A] {
  override def multiplicative: AbGroup[A] = new AbGroup[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
    def inverse(x: A): A = reciprocal(x)
  }
}

object MultiplicativeSemigroup extends MultiplicativeSemigroup1
object MultiplicativeMonoid extends MultiplicativeMonoid0
object MultiplicativeGroup extends MultiplicativeGroup0
object MultiplicativeAbGroup extends MultiplicativeAbGroup0

trait MultiplicativeSemigroup0 {
  implicit def MonoidIsSemigroup[@spec(Int,Long,Float,Double) A](implicit
    m: MultiplicativeMonoid[A]): MultiplicativeSemigroup[A] = m
}

trait MultiplicativeSemigroup1 extends MultiplicativeSemigroup0 {
  implicit def ringIsMultiplicativeSemigroup[@spec(Int, Long, Float, Double) A](implicit
    r: Ring[A]): MultiplicativeSemigroup[A] = r
}

trait MultiplicativeMonoid0 {
  implicit def GroupIsMonoid[@spec(Int,Long,Float,Double) A](implicit
    g: MultiplicativeGroup[A]): MultiplicativeMonoid[A] = g
}

trait MultiplicativeGroup0 {
  implicit def AbGroupIsGroup[@spec(Int,Long,Float,Double) A](implicit
    g: MultiplicativeAbGroup[A]): MultiplicativeGroup[A] = g
}

trait MultiplicativeAbGroup0 {}

import spire.math.{ConvertableTo, ConvertableFrom, Number}

final class MultiplicativeSemigroupOps[A](lhs:A)(implicit ev:MultiplicativeSemigroup[A]) {
  def *(rhs:A): A = macro Ops.binop[A, A]
  def *(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def *(rhs:Double)(implicit ev1:ConvertableTo[A]): A = macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def *(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) * rhs
}

final class MultiplicativeGroupOps[A](lhs:A)(implicit ev:MultiplicativeGroup[A]) {
  def reciprocal() = macro Ops.unop[A]
  def /(rhs:A): A = macro Ops.binop[A, A]
  def /(rhs:Int)(implicit ev1: Ring[A]): A = macro Ops.binopWithLift[Int, Ring[A], A]
  def /(rhs:Double)(implicit ev1:ConvertableTo[A]): A = macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def /(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) / rhs
}
