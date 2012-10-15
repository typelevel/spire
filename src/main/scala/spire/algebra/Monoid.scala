package spire.algebra

trait Monoid[A] extends Semigroup[A] {
  def id: A
}

object Monoid {
  @inline final def apply[A](implicit m: Monoid[A]): Monoid[A] = m
}

class AdditiveMonoid[A](implicit ring:Ring[A]) extends Monoid[A] {
  def id = ring.zero
  def op(x:A, y:A) = ring.plus(x, y)
}

class MultiplicativeMonoid[A](implicit ring:Ring[A]) extends Monoid[A] {
  def id = ring.one
  def op(x:A, y:A) = ring.times(x, y)
}
