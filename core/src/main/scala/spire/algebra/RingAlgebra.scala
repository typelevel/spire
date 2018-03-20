package spire
package algebra


/**
 * A `RingAlgebra` is a module that is also a `Rng`. An example is the Gaussian
 * numbers.
 */
trait RingAlgebra[V, @sp R] extends Any with CModule[V, R] with Rng[V]

object RingAlgebra {
  implicit def ZAlgebra[A](implicit vector0: Ring[A], scalar0: Ring[Int]): ZAlgebra[A] = new ZAlgebra[A] {
    val vector: Ring[A] = vector0
    val scalar: Ring[Int] = scalar0
  }
}

/**
 * Given any `Ring[A]` we can construct a `RingAlgebra[A, Int]`. This is
 * possible since we can define `fromInt` on `Ring` generally.
 */
trait ZAlgebra[V] extends Any with RingAlgebra[V, Int] with Ring[V] {
  implicit def vector: Ring[V]
  implicit def scalar: Ring[Int]

  def zero: V = vector.zero
  def one: V = vector.one
  def negate(v: V): V = vector.negate(v)
  def plus(v: V, w: V): V = vector.plus(v, w)
  override def minus(v: V, w: V): V = vector.minus(v, w)
  def times(v: V, w: V): V = vector.times(v, w)

  def timesl(r: Int, v: V): V = vector.times(vector.fromInt(r), v)

  override def fromInt(n: Int): V = vector.fromInt(n)
}

/**
 * A `FieldAlgebra` is a vector space that is also a `Ring`. An example is the
 * complex numbers.
 */
trait FieldAlgebra[V, @sp(Float, Double) F] extends Any with RingAlgebra[V, F] with VectorSpace[V, F]
