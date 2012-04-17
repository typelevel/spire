package spire.algebra

import spire.math.Eq

trait Semigroup[A] {
  def op(x:A, y:A): A
}

object Semigroup {
  def apply[A](implicit s: Semigroup[A]) = s
}

final class SemigroupOps[A](lhs:A)(implicit ev:Semigroup[A]) {
  def |+|(rhs:A) = ev.op(lhs, rhs)
}
