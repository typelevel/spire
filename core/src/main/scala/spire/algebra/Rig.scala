package spire.algebra

import annotation.tailrec
import scala.{specialized => spec}

import spire.macrosk.Ops


/**
 * Rig is a ring whose additive structure doesn't have an inverse (ie. it is
 * monoid, not a group). Put another way, a Rig is a Ring without a negative.
 */
trait Rig[@spec(Int,Long,Float,Double) A] extends Semiring[A] with AdditiveMonoid[A] with MultiplicativeMonoid[A] {
  override def pow(a:A, n:Int):A =
    if (n < 0) sys.error("illegal exponent: %s" format n)
    else _pow(a, n, one)

  @tailrec private final def _pow(a:A, n:Int, sofar:A):A =
    if (n == 0) sofar
    else if (n % 2 == 1) _pow(times(a, a), n / 2, times(sofar, a))
    else _pow(times(a, a), n / 2, sofar)
}

object Rig {
  @inline final def apply[A](implicit r:Rig[A]): Rig[A] = r
}
