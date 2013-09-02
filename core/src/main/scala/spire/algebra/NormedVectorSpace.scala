package spire.algebra

import spire.std._

import scala.{ specialized => spec }
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

/**
 * A normed vector space is a vector space equipped with a function
 * `norm: V => F`. The main constraint is that the norm of a vector must be
 * scaled linearly when the vector is scaled; that is
 * `norm(k *: v) == k.abs * norm(v)`. Additionally, a normed vector space is
 * also a `MetricSpace`, where `distance(v, w) = norm(v - w)`, and so must
 * obey the triangle inequality.
 *
 * An example of a normed vector space is R^n equipped with the euclidean
 * vector length as the norm.
 */
trait NormedVectorSpace[V, @spec(Int, Long, Float, Double) F]
extends VectorSpace[V, F] with MetricSpace[V, F] {
  def norm(v: V): F

  def normalize(v: V): V = divr(v, norm(v))
  def distance(v: V, w: V): F = norm(minus(v, w))
}

object NormedVectorSpace extends NormedVectorSpace0 with NormedVectorSpaceFunctions {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: NormedVectorSpace[V, R]) = V
}

trait NormedVectorSpace0 {
  implicit def InnerProductSpaceIsNormedVectorSpace[V, @spec(Int, Long, Float, Double) F](implicit
    space: InnerProductSpace[V, F], nroot: NRoot[F]): NormedVectorSpace[V, F] = space.normed
}

trait NormedVectorSpaceFunctions {
  def max[A, CC[A] <: SeqLike[A, CC[A]]](implicit field0: Field[A], order0: Order[A],
      signed0: Signed[A], cbf0: CanBuildFrom[CC[A], A, CC[A]]): NormedVectorSpace[CC[A], A] =
    new SeqMaxNormedVectorSpace[A, CC[A]]

  def Lp[A, CC[A] <: SeqLike[A, CC[A]]](p: Int)(implicit field0: Field[A], nroot0: NRoot[A],
      signed0: Signed[A], cbf0: CanBuildFrom[CC[A], A, CC[A]]): NormedVectorSpace[CC[A], A] =
    new SeqLpNormedVectorSpace[A, CC[A]](p)
}
