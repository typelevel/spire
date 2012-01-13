package numerics.math

trait Monoid[A] extends Semigroup[A] {
  def identity: A
}

class AdditiveMonoid[A:Ring] extends Monoid[A] {
  private val ring = implicitly[Ring[A]]
  def identity = ring.zero
  def op(x:A, y:A) = ring.plus(x, y)
  def equiv(x:A, y:A) = ring.equiv(x, y)
  def nequiv(x:A, y:A) = ring.nequiv(x, y)
}

class MultiplicativeMonoid[A:Ring] extends Monoid[A] {
  private val ring = implicitly[Ring[A]]
  def identity = ring.one
  def op(x:A, y:A) = ring.times(x, y)
  def equiv(x:A, y:A) = ring.equiv(x, y)
  def nequiv(x:A, y:A) = ring.nequiv(x, y)
}
