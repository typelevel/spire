package spire.algebra

import spire.macrosk.Ops

/**
 * A group is a monoid where each element has an inverse.
 */
trait Group[A] extends Monoid[A] {
  def inverse(a: A): A
}

object Group {
  @inline final def apply[A](implicit ev: Group[A]) = ev
}

/**
 * An abelian group is a group whose operation is commutative.
 */
trait AbGroup[A] extends Group[A]

final class GroupOps[A](lhs:A)(implicit ev:Group[A]) {
  def inverse() = macro Ops.unop[A]
}
