package spire.algebra

import scala.{ specialized => spec }
import spire.macrosk.Ops

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

final class MultiplicativeSemigroupOps[A](lhs:A)(implicit ev:MultiplicativeSemigroup[A]) {
  def *(rhs:A): A = macro Ops.binop[A, A]
}

final class MultiplicativeGroupOps[A](lhs:A)(implicit ev:MultiplicativeGroup[A]) {
  def /(rhs:A): A = macro Ops.binop[A, A]
}
