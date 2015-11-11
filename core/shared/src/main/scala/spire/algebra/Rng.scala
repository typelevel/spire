package spire
package algebra

/**
 * Rng is a ring whose multiplicative structure doesn't have an identity
 * (i.e. it is semigroup, not a monoid). Put another way, a Rng is a Ring
 * without an identity.
 */
trait Rng[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Semiring[A] with AdditiveAbGroup[A]

object Rng {
  @inline final def apply[A](implicit r:Rng[A]):Rng[A] = r
}
