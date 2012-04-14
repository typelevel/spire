package spire.algebra

trait Monoid[A] extends Semigroup[A] {
  def identity: A
}

object Monoid {
  def apply[A](implicit m: Monoid[A]): Monoid[A] = m
}

class MultiplicativeMonoid[A:Ring] extends Monoid[A] {
  private val ring = implicitly[Ring[A]]
  def identity = ring.one
  def op(x:A, y:A) = ring.times(x, y)
}
