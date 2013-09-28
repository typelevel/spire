package spire.algebra

import scala.{ specialized => spec }

/**
 * A group is a monoid where each element has an inverse.
 */
trait Group[@spec(Byte, Short, Int, Long, Float, Double) A] extends Monoid[A] {
  def inverse(a: A): A
  def opInverse(a: A, b: A): A = op(a, inverse(b))
}

object Group {
  @inline final def apply[A](implicit ev: Group[A]): Group[A] = ev
  @inline final def additive[A](implicit A: AdditiveGroup[A]): Group[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeGroup[A]): Group[A] = A.multiplicative

  /**
   * Return `a` appended to itself `n` times. If `n` is negative, then
   * this returns `-a` appended to itself `n` times.
   */
  final def sumn[@spec(Byte, Short, Int, Long, Float, Double) A](a: A, n: Int)(implicit A: Group[A]):A =
    if (n == Int.MinValue) A.op(Monoid.sumn(A.inverse(a), Int.MaxValue), A.inverse(a))
    else if (n < 0) Monoid.sumn(A.inverse(a), -n)
    else Monoid.sumn(a, n)
}

/**
 * An abelian group is a group whose operation is commutative.
 */
trait AbGroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends Group[A] with CMonoid[A]

object AbGroup {
  @inline final def apply[A](implicit ev: AbGroup[A]): AbGroup[A] = ev
  @inline final def additive[A](implicit A: AdditiveAbGroup[A]): AbGroup[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeAbGroup[A]): AbGroup[A] = A.multiplicative
}
