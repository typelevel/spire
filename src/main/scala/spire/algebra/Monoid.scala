package spire.algebra

import scala.{ specialized => spec }

/**
 * A monoid is a semigroup with a special identity element.
 */
trait Monoid[@spec(Int,Long,Float,Double) A] extends Semigroup[A] {
  def id: A
}

object Monoid {
  @inline final def apply[A](implicit m: Monoid[A]): Monoid[A] = m
}
