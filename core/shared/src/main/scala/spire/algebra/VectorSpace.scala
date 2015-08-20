package spire.algebra

import scala.{ specialized => spec }

/**
 * A vector space is a group `V` that can be multiplied by scalars in `F` that
 * lie in a field. Scalar multiplication must distribute over vector addition
 * (`x *: (v + w) === x *: v + x *: w`) and scalar addition
 * (`(x + y) *: v === x *: v + y *: v`). Scalar multiplication by 1 in `F`
 * is an identity function (`1 *: v === v`). Scalar multiplication is
 * "associative" (`x *: y *: v === (x * y) *: v`).
 */
trait VectorSpace[V, @spec(Int, Long, Float, Double) F] extends Any with Module[V, F] {
  implicit def scalar: Field[F]

  def divr(v: V, f: F): V = timesl(scalar.reciprocal(f), v)
}

object VectorSpace {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: VectorSpace[V, R]): VectorSpace[V, R] = V
}
