package spire.algebra

import spire.macrosk.Ops

import scala.{ specialized => spec }

/**
 * A group is a monoid where each element has an inverse.
 */
trait Group[@spec(Int,Long,Float,Double) A] extends Monoid[A] {
  def inverse(a: A): A
}

object Group {
  @inline final def apply[A](implicit ev: Group[A]) = ev

  @inline final def additive[A](implicit A: AdditiveGroup[A]) =  A.additive

  @inline final def multiplicative[A](implicit A: MultiplicativeGroup[A]) = A.multiplicative
}

/**
 * An abelian group is a group whose operation is commutative.
 */
trait AbGroup[@spec(Int,Long,Float,Double) A] extends Group[A]

object AbGroup {
  @inline final def apply[A](implicit ev: AbGroup[A]) = ev

  @inline final def additive[A](implicit A: AdditiveAbGroup[A]) =  A.additive

  @inline final def multiplicative[A](implicit A: MultiplicativeAbGroup[A]) = A.multiplicative
}

final class GroupOps[A](lhs:A)(implicit ev:Group[A]) {
  def inverse() = macro Ops.unop[A]
}
