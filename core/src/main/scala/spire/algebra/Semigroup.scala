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
  @inline final def additive[A](implicit A: AdditiveSemigroup[A]) =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeSemigroup[A]) = A.multiplicative
}
