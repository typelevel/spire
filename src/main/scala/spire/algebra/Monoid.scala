package spire.algebra

/**
 * A monoid is a semigroup with a special identity element.
 */
trait Monoid[A] extends Semigroup[A] {
  def id: A
}

object Monoid {
  @inline final def apply[A](implicit m: Monoid[A]): Monoid[A] = m
}
