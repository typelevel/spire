package spire.algebra

import scala.{ specialized => spec }

/**
 * A semigroup is any set `A` with an associative operation (`op`).
 */
trait Semigroup[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A] {
  def op(x: A, y: A): A
}

object Semigroup {
  @inline final def apply[A](implicit s: Semigroup[A]) = s

  /**
   * If there exists an implicit `AdditiveSemigroup[A]`, this returns a
   * `Semigroup[A]` using `plus` for `op`.
   */
  @inline final def additive[A](implicit A: AdditiveSemigroup[A]) =  A.additive

  /**
   * If there exists an implicit `MultiplicativeSemigroup[A]`, this returns a
   * `Semigroup[A]` using `times` for `op`.
   */
  @inline final def multiplicative[A](implicit A: MultiplicativeSemigroup[A]) = A.multiplicative
}
