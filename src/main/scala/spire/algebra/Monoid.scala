package spire.algebra

trait Monoid[A] extends Semigroup[A] {
  def id: A
}

object Monoid {
  @inline final def apply[A](implicit m: Monoid[A]): Monoid[A] = m
}

class AdditiveMonoid[A](implicit rig:Rig[A]) extends Monoid[A] {
  def id = rig.zero
  def op(x:A, y:A) = rig.plus(x, y)
}

class MultiplicativeMonoid[A](implicit rig:Rig[A]) extends Monoid[A] {
  def id = rig.one
  def op(x:A, y:A) = rig.times(x, y)
}
