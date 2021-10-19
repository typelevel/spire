package spire
package algebra

/**
 * A `RingAssociativeAlgebra` is a R-module that is also a `Ring`. An example is the Gaussian numbers, the quaternions,
 * etc...
 *
 * The scalar multiplication satisfies, for r in R, and x, y in V:
 *
 *   1. r *: (x * y) = (r *: x) * y = x * (r *: y)
 *
 * TODO: verify the definition, in particular the requirements for Ring[V] (and not Rng[V])
 */
trait RingAssociativeAlgebra[V, @sp R] extends Any with CModule[V, R] with Ring[V]

object RingAssociativeAlgebra {
  @inline final def apply[V, @sp R](implicit V: RingAssociativeAlgebra[V, R]): RingAssociativeAlgebra[V, R] = V
}

/**
 * Given any `Ring[A]` we can construct a `RingAlgebra[A, Int]`. This is possible since we can define `fromInt` on
 * `Ring` generally.
 */
trait ZAlgebra[V] extends Any with RingAssociativeAlgebra[V, Int] with Ring[V] {
  implicit def vector: Ring[V]
  implicit def scalar: CRing[Int]

  def zero: V = vector.zero
  def one: V = vector.one
  def negate(v: V): V = vector.negate(v)
  def plus(v: V, w: V): V = vector.plus(v, w)
  override def minus(v: V, w: V): V = vector.minus(v, w)
  def times(v: V, w: V): V = vector.times(v, w)

  def timesl(r: Int, v: V): V = vector.times(vector.fromInt(r), v)

  override def fromInt(n: Int): V = vector.fromInt(n)
}

object ZAlgebra {
  def apply[A](implicit vector0: Ring[A], scalar0: CRing[Int]): ZAlgebra[A] = new ZAlgebra[A] {
    val vector: Ring[A] = vector0
    val scalar: CRing[Int] = scalar0
  }
}

/**
 * A `FieldAlgebra` is a vector space that is also a `Ring`. An example is the complex numbers.
 */
trait FieldAssociativeAlgebra[V, @sp(Float, Double) F]
    extends Any
    with RingAssociativeAlgebra[V, F]
    with VectorSpace[V, F]

object FieldAssociativeAlgebra {
  @inline final def apply[V, @sp R](implicit V: FieldAssociativeAlgebra[V, R]): FieldAssociativeAlgebra[V, R] = V
}
