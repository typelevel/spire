package spire.algebra

import scala.{ specialized => spec }

/**
 * A group is a monoid where each element has an inverse.
 */
trait Group[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any with Monoid[A] {

  /**
   * Return the inverse of `a`.
   */
  def inverse(a: A): A

  /**
   * Combine `a` with the inverse of `b`.
   */
  def opInverse(a: A, b: A): A = op(a, inverse(b))

  /**
   * Return `a` combined with itself `n` times.
   */
  override def combinen(a: A, n: Int): A =
    if (n == Int.MinValue) op(combinen(inverse(a), Int.MaxValue), inverse(a))
    else if (n < 0) combinen(inverse(a), -n)
    else if (n == 0) id
    else if (n == 1) a
    else combinenAboveOne(a, n)
}

object Group {
  @inline final def apply[A](implicit ev: Group[A]): Group[A] = ev
  @inline final def additive[A](implicit A: AdditiveGroup[A]): Group[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeGroup[A]): Group[A] = A.multiplicative
}

/**
 * An abelian group is a group whose operation is commutative.
 */
trait AbGroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any with Group[A] with CMonoid[A]

object AbGroup {
  @inline final def apply[A](implicit ev: AbGroup[A]): AbGroup[A] = ev
  @inline final def additive[A](implicit A: AdditiveAbGroup[A]): AbGroup[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeAbGroup[A]): AbGroup[A] = A.multiplicative
}
