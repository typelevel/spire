package spire
package algebra


/**
 * A monoid is a semigroup with an identity named empty. A monoid is a specialization of a
 * semigroup, so its operation must be associative. Additionally,
 * `combine(x, empty) == combine(empty, x) == x`. For example, if we have `Monoid[String]`,
 * with `combine` as string concatenation, then `empty = ""`.
 */
trait Monoid[@sp(Int, Long, Float, Double) A] extends Any with Semigroup[A] {

  /**
   * Return the identity element for this monoid.
   */
  def empty: A

  /**
    * Tests if `a` is the identity.
    */
  def isEmpty(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, empty)

  /**
   * Return `a` combined with itself `n` times.
   */
  override def combineN(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated combining for monoids must have repetitions >= 0")
    else if (n == 0) empty
    else repeatedCombineN(a, n)

  /**
   *  Given a sequence of `as`, combine them using the monoid and return the total.
   */
  def combineAll(as: TraversableOnce[A]): A = as.foldLeft(empty)(combine)

  override def combineAllOption(as: TraversableOnce[A]): Option[A] =
    if (as.isEmpty) None else Some(combineAll(as))

}

object Monoid {
  @inline final def apply[A](implicit m: Monoid[A]): Monoid[A] = m
}

/**
 * CMonoid represents a commutative monoid.
 *
 * A monoid is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CMonoid[@sp(Int, Long, Float, Double) A] extends Any with Monoid[A] with CSemigroup[A]

object CMonoid {
  @inline final def apply[A](implicit ev: CMonoid[A]): CMonoid[A] = ev
}
