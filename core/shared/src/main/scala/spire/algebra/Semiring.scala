package spire
package algebra

/**
 * Semiring is a ring without identities or an inverse. Thus, it has no
 * negation, zero, or one.
 *
 * A Semiring with an additive inverse (-) is a Rng.
 * A Semiring with additive and multiplicative identities (0 and 1) is a Rig.
 * A Semiring with all of the above is a Ring.
 */
trait Semiring[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveMonoid[A] with MultiplicativeSemigroup[A]

object Semiring {
  @inline final def apply[A](implicit r:Semiring[A]):Semiring[A] = r
}
