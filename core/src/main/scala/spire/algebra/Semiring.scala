package spire.algebra

import annotation.tailrec
import scala.{specialized => spec}


/**
 * Semiring is a ring without identities or an inverse. Thus, it has no
 * negation, zero, or one.
 *
 * A Semiring with an additive inverse (-) is a Rng.
 * A Semiring with additive and multiplicative identities (0 and 1) is a Rig.
 * A Semiring with all of the above is a Ring.
 */
trait Semiring[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveMonoid[A] with MultiplicativeSemigroup[A] {
  /**
   * Returns `a` multiplied with itself `n` times. For instance,
   * `a pow 3 === a * a * a`. Since this is a semiring, there is no notion of
   * a multiplicative identity, and so the exponent must be positive.
   */
  def pow(a:A, n:Int):A =
    if (n > 0) multiplicative.sumn(a, n)
    else throw new IllegalArgumentException(s"Illegal non-positive exponent $n to Semiring#pow")
}

object Semiring {
  @inline final def apply[A](implicit r:Semiring[A]):Semiring[A] = r
}
