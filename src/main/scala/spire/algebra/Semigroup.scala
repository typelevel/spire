package spire.algebra

import spire.macrosk.Ops

trait Semigroup[A] {
  def op(x:A, y:A): A
}

object Semigroup {
  @inline final def apply[A](implicit s: Semigroup[A]) = s
}

final class SemigroupOps[A](lhs:A)(implicit ev:Semigroup[A]) {
  def |+|(rhs:A) = macro Ops.binop[A, A]
}
