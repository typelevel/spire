package spire.algebra

import scala.{ specialized => spec }
import spire.macrosk.Ops

trait AdditiveMonoid[@spec(Int,Long,Float,Double) A] {
  def additive: Monoid[A] = new Monoid[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
  }

  def zero: A
  def plus(x: A, y: A): A
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

trait AdditiveMonoid0 {
  implicit def moduleIsAdditiveMonoid[V, @spec(Int, Long, Float, Double) R](implicit
    m: Module[V, R]): AdditiveMonoid[V] = m
}

object AdditiveMonoid extends AdditiveMonoid0 {
  implicit def ringIsAdditiveMonoid[@spec(Int, Long, Float, Double) A](implicit
    r: Ring[A]): AdditiveMonoid[A] = r
}

final class AdditiveMonoidOps[A](lhs:A)(implicit ev:AdditiveMonoid[A]) {
  def +(rhs:A): A = macro Ops.binop[A, A]
}

final class AdditiveGroupOps[A](lhs:A)(implicit ev:AdditiveGroup[A]) {
  def unary_-() = macro Ops.unop[A]
  def -(rhs:A): A = macro Ops.binop[A, A]
}
