package spire.algebra

import spire.macrosk.Ops

import scala.{ specialized => spec }

/**
 * A group is a monoid where each element has an inverse.
 */
trait Group[@spec(Int,Long,Float,Double) A] extends Monoid[A] {
  def inverse(a: A): A
}

object Group extends Group0 {
  @inline final def apply[A](implicit ev: Group[A]) = ev
}

/**
 * An abelian group is a group whose operation is commutative.
 */
trait AbGroup[@spec(Int,Long,Float,Double) A] extends Group[A]

object AbGroup extends AbGroupProductImplicits {
  @inline final def apply[A](implicit ev: AbGroup[A]) = ev
}

final class GroupOps[A](lhs:A)(implicit ev:Group[A]) {
  def inverse() = macro Ops.unop[A]
}

trait Group0 extends GroupProductImplicits {
  implicit def abGroup[A: AbGroup]: Group[A] = AbGroup[A]
}
