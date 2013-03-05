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
trait Semiring[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveSemigroup[A] with MultiplicativeSemigroup[A] {
  // NOTE: that for a Semiring, the exponent must be > 0.
  def pow(a:A, n:Int):A =
    if (n < 1) sys.error("illegal exponent: %s" format n)
    else _pow(a, n - 1, a)

  @tailrec private final def _pow(a:A, n:Int, sofar:A):A =
    if (n == 0) sofar
    else if (n % 2 == 1) _pow(times(a, a), n / 2, times(sofar, a))
    else _pow(times(a, a), n / 2, sofar)
}

object Semiring {
  @inline final def apply[A](implicit r:Semiring[A]):Semiring[A] = r
}
