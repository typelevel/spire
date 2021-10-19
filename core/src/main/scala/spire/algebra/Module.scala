package spire
package algebra

/**
 * A left module is a generalization of a vector space over a field, where the scalars are the elements of a ring (not
 * necessarily commutative).
 *
 * A left module has left multiplication by scalars. Let V be an abelian group (with additive notation) and R the scalar
 * ring, we have the following laws for x, y in V and r, s in R:
 *
 *   1. r *: (x + y) = r *: x + r *: y 2. (r + s) *: x = r *: x + s *: x 3. (r * s) *: x = r *: (s *: x) 4. R.one * x =
 *      x
 *
 * (see https://en.wikipedia.org/wiki/Module_(mathematics) )
 *
 * @tparam V
 *   Abelian group type
 * @tparam R
 *   Scalar type
 */
trait LeftModule[V, @sp(Int, Long, Float, Double) R] extends Any with AdditiveAbGroup[V] {
  implicit def scalar: Ring[R] // note: we require a ring with identity (see https://arxiv.org/pdf/1404.0135.pdf)

  def timesl(r: R, v: V): V
}

object LeftModule {
  @inline final def apply[V, @sp(Int, Long, Float, Double) R](implicit V: LeftModule[V, R]): LeftModule[V, R] = V
}

/**
 * A right module is a generalization of a vector space over a field, where the scalars are the elements of a ring (not
 * necessarily commutative).
 *
 * A right module has right multiplication by scalars. Let V be an abelian group (with additive notation) and R the
 * scalar ring, we have the following laws for x, y in V and r, s in R:
 *
 *   1. (x + y) :* r = x :* r + y :* r 2. x :* (r + s) = x :* r + x :* s 3. x :* (r * s) = (x :* r) :* s 4. x :* R.one =
 *      x
 *
 * @tparam V
 *   Abelian group type
 * @tparam R
 *   Scalar type
 */
trait RightModule[V, @sp(Int, Long, Float, Double) R] extends Any with AdditiveAbGroup[V] {
  implicit def scalar: Ring[R] // note: we require a ring with identity (see https://arxiv.org/pdf/1404.0135.pdf)

  def timesr(v: V, r: R): V
}

object RightModule {
  @inline final def apply[V, @sp(Int, Long, Float, Double) R](implicit V: RightModule[V, R]): RightModule[V, R] = V
}

// we skip the implementation of bimodules due to the explosion of specialized type parameters

/**
 * A module over a commutative ring has by definition equivalent left and right modules.
 *
 * In addition to the laws above 1-5 left and 1-5 right, we have:
 *
 * 6. (r *: x) :* s = r *: (x :* s)
 *
 * @tparam V
 *   Abelian group type
 * @tparam R
 *   Scalar type
 */
trait CModule[V, @sp(Int, Long, Float, Double) R] extends Any with LeftModule[V, R] with RightModule[V, R] {
  implicit def scalar: CRing[R]

  override def timesr(v: V, r: R): V = timesl(r, v)
}

object CModule {
  @inline final def apply[V, @sp(Int, Long, Float, Double) R](implicit V: CModule[V, R]): CModule[V, R] = V
}
