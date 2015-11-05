package spire
package algebra


/**
 * A monoid is a semigroup with an identity. A monoid is a specialization of a
 * semigroup, so its operation must be associative. Additionally,
 * `op(x, id) == op(id, x) == x`. For example, if we have `Monoid[String]`,
 * with `op` as string concatenation, then `id = ""`.
 */
trait Monoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with Semigroup[A] {

  /**
   * Return the identity element for this monoid.
   */
  def id: A

  /**
    * Tests if `a` is the identity.
    */
  def isId(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, id)

  /**
   * Return `a` combined with itself `n` times.
   */
  override def combinen(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated combination for monoids must have repetitions >= 0")
    else if (n == 0) id
    else if (n == 1) a
    else combinenAboveOne(a, n)

  /**
   *  Given a sequence of `as`, combine them using the monoid and return the total.
   */
  def combine(as: TraversableOnce[A]): A = as.aggregate(id)(op, op)
}

object Monoid {
  @inline final def apply[A](implicit m: Monoid[A]): Monoid[A] = m

  /**
   * If an implicit `AdditiveMonoid[A]` exists, then it is converted to a plain
   * `Monoid[A]`.
   */
  @inline final def additive[A](implicit A: AdditiveMonoid[A]): Monoid[A] = A.additive

  /**
   * If an implicit `MultiplicativeMonoid[A]` exists, then it is converted to a
   * plain `Monoid[A]`.
   */
  @inline final def multiplicative[A](implicit A: MultiplicativeMonoid[A]): Monoid[A] = A.multiplicative

}

/**
 * CMonoid represents a commutative monoid.
 *
 * A monoid is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CMonoid[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with Monoid[A] with CSemigroup[A]

object CMonoid {
  @inline final def apply[A](implicit ev: CMonoid[A]): CMonoid[A] = ev
  @inline final def additive[A](implicit A: AdditiveCMonoid[A]): CMonoid[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeCMonoid[A]): CMonoid[A] = A.multiplicative
}
