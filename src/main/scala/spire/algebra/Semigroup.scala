package spire.algebra

import spire.macrosk.Ops

import scala.{ specialized => spec }

/**
 * A semigroup is any set `A` with an associative operation (`op`).
 */
trait Semigroup[@spec(Int,Long,Float,Double) A] {
  def op(x:A, y:A): A
}

object Semigroup extends Semigroup0 {
  @inline final def apply[A](implicit s: Semigroup[A]) = s
}

trait Semigroup0 {
  implicit def monoid[A: Monoid]: Semigroup[A] = Monoid[A]
}

final class SemigroupOps[A](lhs:A)(implicit ev:Semigroup[A]) {
  def |+|(rhs:A) = macro Ops.binop[A, A]
}
