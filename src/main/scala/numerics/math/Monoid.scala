package numerics.math

trait Monoid[A] extends Semigroup[A] {
  def identity: A
}
