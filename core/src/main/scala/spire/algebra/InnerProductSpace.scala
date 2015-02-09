package spire.algebra

import scala.{ specialized => spec }
import scala.annotation.tailrec

trait InnerProductSpace[V, @spec(Int, Long, Float, Double) F] extends Any with VectorSpace[V, F] { self =>
  def dot(v: V, w: V): F

  def normed(implicit ev: NRoot[F]): NormedVectorSpace[V, F] =
    new NormedInnerProductSpace()(this, ev)
}

object InnerProductSpace {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: InnerProductSpace[V, R]) = V

  implicit def RealInnerProductSpace[V, @spec(Int, Long, Float, Double) K]
    (implicit V: VectorSpace[V, K], basis: Basis[V, K],
      K: Rng[K], K0: IsReal[K]): InnerProductSpace[V, K] =
    new RealInnerProductSpace()(V, basis, K, K0)
}

@SerialVersionUID(1L)
private[algebra] class NormedInnerProductSpace[V, @spec(Float, Double) F]
    (implicit val vectorSpace: InnerProductSpace[V, F], val nroot: NRoot[F])
    extends NormedVectorSpace[V, F] with PassThroughVectorSpace[V, F] {
  def norm(v: V): F = nroot.sqrt(vectorSpace.dot(v, v))
}

private[algebra] class RealInnerProductSpace[V, @spec(Int, Long, Float, Double) K]
    (implicit val vectorSpace: VectorSpace[V, K], basis: Basis[V, K], K: Rng[K], K0: IsReal[K])
    extends InnerProductSpace[V, K] with PassThroughVectorSpace[V, K] {
  private val K1 = K.additive

  def dot(v: V, w: V): K =
    basis.zipFoldMap(v, w)(K.times)(K1)
}
