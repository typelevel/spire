package spire
package algebra

/**
 * A type class used to determine equality between 2 instances of the same
 * type. Any 2 instances `x` and `y` are equal if `eqv(x, y)` is `true`.
 * Moreover, `eqv` should form an equivalence relation.
 */
trait Eq[@sp A] extends Any {
  /** Returns `true` if `x` and `y` are equivalent, `false` otherwise. */
  def eqv(x:A, y:A): Boolean

  /** Returns `false` if `x` and `y` are equivalent, `true` otherwise. */
  def neqv(x:A, y:A): Boolean = !eqv(x, y)

  /**
   * Constructs a new `Eq` instance for type `B` where 2 elements are
   * equivalent iff `eqv(f(x), f(y))`.
   */
  def on[@sp B](f:B => A): Eq[B] = new MappedEq(this)(f)
}

private[algebra] class MappedEq[@sp A, @sp B](eq: Eq[B])(f: A => B) extends Eq[A] {
  def eqv(x: A, y: A): Boolean = eq.eqv(f(x), f(y))
}

object Eq {
  def apply[A](implicit e:Eq[A]):Eq[A] = e

  def by[@sp A, @sp B](f:A => B)(implicit e:Eq[B]): Eq[A] = new MappedEq(e)(f)
}
