package spire.algebra

import scala.{ specialized => spec }


/**
 * A trait for things that have some notion of sign and the ability to ensure
 * something has a positive sign.
 */
trait Signed[@spec(Double, Float, Int, Long) A] {
  def sign(a: A): Sign = Sign(signum(a))
  def signum(a: A): Int
  def abs(a: A): A
}

trait SignedLow {
  implicit def orderedRingIsSigned[A: Order: Ring]: Signed[A] = new OrderedRingIsSigned[A]
}

object Signed extends SignedLow {
  def apply[A](implicit s: Signed[A]): Signed[A] = s
}

class OrderedRingIsSigned[A](implicit o: Order[A], r: Ring[A]) extends Signed[A] {
  def signum(a: A) = o.compare(a, r.zero)
  def abs(a: A) = if (signum(a) < 0) r.negate(a) else a
}
