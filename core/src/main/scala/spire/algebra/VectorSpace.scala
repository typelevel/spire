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
trait VectorSpace[V, @spec(Int, Long, Float, Double) K] extends AdditiveAbGroup[V] {
  def timesl(r: K, v: V): V
  def timesr(v: V, r: K): V = timesl(r, v)
  def divr(v: V, r: K)(implicit K: Field[K]): V = timesr(v, K.reciprocal(r))
}

object VectorSpace {
  @inline final def apply[V, @spec(Int,Long,Float,Double) K](implicit V: VectorSpace[V, K]) = V

  final def fromGroup[V: Group]: VectorSpace[V, Int] = new ZModule(Group[V])

  //implicit def IdentityVectorSpace[@spec(Int,Long,Float,Double) V](implicit scalar: Rng[V]) =
  //  new IdentityVectorSpace[V](scalar)
}

@SerialVersionUID(1L)
private[algebra] final class IdentityVectorSpace[@spec(Int,Long,Float,Double) V](scalar: Rng[V]) extends VectorSpace[V, V] {
  def zero = scalar.zero
  def negate(v: V) = scalar.negate(v)
  def plus(v: V, w: V): V = scalar.plus(v, w)
  override def minus(v: V, w: V): V = scalar.minus(v, w)
  override def divr(v: V, r: V)(implicit V: Field[V]): V = V.div(v, r)

  def timesl(r: V, v: V): V = scalar.times(r, v)
}

@SerialVersionUID(1L)
private[algebra] final class ZModule[V](vector: Group[V]) extends VectorSpace[V, Int] {
  def zero: V = vector.id
  def negate(v: V): V = vector.inverse(v)
  def plus(v: V, w: V): V = vector.op(v, w)
  def timesl(k: Int, v: V): V = vector.sumn(v, k)
}

private[algebra] trait PassThroughVectorSpace[V, K] extends VectorSpace[V, K] {
  val vectorSpace: VectorSpace[V, K]

  def zero: V = vectorSpace.zero
  def plus(v: V, w: V): V = vectorSpace.plus(v, w)
  def negate(v: V): V = vectorSpace.negate(v)
  override def minus(v: V, w: V): V = vectorSpace.minus(v, w)
  def timesl(f: K, v: V): V = vectorSpace.timesl(f, v)
  override def divr(v: V, f: K)(implicit K: Field[K]): V = vectorSpace.divr(v, f)
}
