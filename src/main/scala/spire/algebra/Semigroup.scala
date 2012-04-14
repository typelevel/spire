package spire.algebra

import spire.math.Eq

trait Semigroup[A] extends Eq[A] {
  def op(x:A, y:A): A
}

final class SemigroupOps[A](lhs:A)(implicit ev:Semigroup[A]) {
  def |+|(rhs:A) = ev.op(lhs, rhs)
}
