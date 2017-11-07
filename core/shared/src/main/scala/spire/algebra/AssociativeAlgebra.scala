package spire
package algebra

/**
  * A `RingAssociativeAlgebra` is a R-module that is also a `Ring`. An example is the Gaussian
  * numbers, the quaternions, etc...
  *
  * The scalar multiplication satisfies, for r in R, and x, y in V:
  *
  * 1. r *: (x * y) = (r *: x) * y = x * (r *: y)
  *
  * TODO: verify the definition, in particular the requirements for Ring[V] (and not Rng[V])
  */
trait RingAssociativeAlgebra[V, @sp R] extends Any with CModule[V, R] with Ring[V]

object RingAssociativeAlgebra {
  @inline final def apply[V, @sp R](implicit V: RingAssociativeAlgebra[V, R]): RingAssociativeAlgebra[V, R] = V
}

/**
 * A `FieldAlgebra` is a vector space that is also a `Ring`. An example is the
 * complex numbers.
 */
trait FieldAssociativeAlgebra[V, @sp(Float, Double) F] extends Any with RingAssociativeAlgebra[V, F] with VectorSpace[V, F]

object FieldAssociativeAlgebra {
  @inline final def apply[V, @sp R](implicit V: FieldAssociativeAlgebra[V, R]): FieldAssociativeAlgebra[V, R] = V
}
